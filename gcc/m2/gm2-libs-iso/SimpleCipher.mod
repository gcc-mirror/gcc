(* SimpleCipher.mod implements a pegalogical caesar cipher.

Copyright (C) 2008-2025 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  *)

IMPLEMENTATION MODULE SimpleCipher ;


FROM SYSTEM IMPORT ADDRESS, ADR, CARDINAL8, LOC ;
FROM RTio IMPORT GetDeviceId ;
FROM RTdata IMPORT ModuleId, MakeModuleId, InitData, GetData, KillData ;
FROM IOLink IMPORT DeviceId, DeviceTable, DeviceTablePtr, DeviceTablePtrValue, AllocateDeviceId, RAISEdevException ;
FROM IOChan IMPORT ChanExceptions ;
FROM IOConsts IMPORT ReadResults ;
FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM ASCII IMPORT nul, lf ;
FROM Strings IMPORT Insert, Append ;
FROM CharClass IMPORT IsLower, IsUpper, IsNumeric ;


TYPE
   PtrToLoc   = POINTER TO LOC ;
   PtrToChar  = POINTER TO CHAR ;
   CipherInfo = POINTER TO RECORD
                              key  : INTEGER ;
                              lower: DeviceTable ;
                           END ;


VAR
   mid: ModuleId ;


(*
   RotateChar - 
*)

PROCEDURE RotateChar (ch, lower, upper: CHAR; key: INTEGER) : CHAR ;
VAR
   r: INTEGER ;
BEGIN
   r := VAL(INTEGER, ORD(upper)-ORD(lower))+1 ;
   IF key<0
   THEN
      RETURN( RotateChar(ch, lower, upper, r-key) )
   ELSE
      IF key>r
      THEN
         key := key MOD r
      END ;
      (* key is now positive and within a sensible range *)
      IF ORD(ch)+VAL(CARDINAL, key)>ORD(upper)
      THEN
         RETURN( CHR((ORD(ch)+VAL(CARDINAL, key))-VAL(CARDINAL, r)) )
      ELSE
         RETURN( CHR(ORD(ch)+VAL(CARDINAL, key)) )
      END
   END
END RotateChar ;


(*
   encryptChar - encrypts, ch, using Caesar cipher.  Only
                 characters [A-Z][a-z][0-9] are encrypted.
                 Also these character ranges are only rotated
                 around their own range.
*)

PROCEDURE encryptChar (ch: CHAR; key: INTEGER) : CHAR ;
BEGIN
   IF IsLower(ch)
   THEN
      RETURN( RotateChar(ch, 'a', 'z', key) )
   ELSIF IsUpper(ch)
   THEN
      RETURN( RotateChar(ch, 'A', 'Z', key) )
   ELSIF IsNumeric(ch)
   THEN
      RETURN( RotateChar(ch, '0', '9', key) )
   ELSE
      RETURN( ch )
   END
END encryptChar ;


(*
   decryptChar - decrypts, ch, using Caesar cipher.  Only
                 characters [A-Z][a-z][0-9] are decrypted.
                 Also these character ranges are only rotated
                 around their own range.
*)

PROCEDURE decryptChar (ch: CHAR; key: INTEGER) : CHAR ;
BEGIN
   RETURN( encryptChar(ch, -key) )
END decryptChar ;


(*
   RotateLoc - 
*)

PROCEDURE RotateLoc (cid: ChanId;
                     did: DeviceId;
                     l: LOC; key: INTEGER) : LOC ;
VAR
   i, u: INTEGER ;
   c: CARDINAL8 ;
BEGIN
   IF SIZE(l)#SIZE(c)
   THEN
      RAISEdevException(cid, did, notAvailable, 'SimpleCipher: unable to cipher LOCs of this size')
   ELSE
      IF key<0
      THEN
         RETURN( RotateLoc(cid, did, l, -key+VAL(INTEGER, MAX(CARDINAL8))) )
      ELSE
         IF key>VAL(INTEGER, MAX(CARDINAL8))
         THEN
            key := key MOD (VAL(INTEGER, MAX(CARDINAL8))+1)
         END ;
         c := VAL(CARDINAL8, l) ;
         u := VAL(INTEGER, MAX(CARDINAL8))+1 ;
         IF u-VAL(INTEGER, c)>key
         THEN
            INC(c, key)
         ELSE
            c := key-(u-VAL(INTEGER, c))
         END ;
         RETURN( VAL(LOC, c) )
      END
   END
END RotateLoc ;


(*
   encryptLoc - encrypts, l, by, key.
*)

PROCEDURE encryptLoc (cid: ChanId; did: DeviceId; l: LOC; key: INTEGER) : LOC ;
BEGIN
   RETURN( RotateLoc(cid, did, l, key) )
END encryptLoc ;


(*
   decryptLoc - decrypts, l, by, key.
*)

PROCEDURE decryptLoc (cid: ChanId; did: DeviceId; l: LOC; key: INTEGER) : LOC ;
BEGIN
   RETURN( RotateLoc(cid, did, l, -key) )
END decryptLoc ;


PROCEDURE dolook (d: DeviceTablePtr;
                  VAR ch: CHAR; VAR r: ReadResults) ;
VAR
   c: CipherInfo ;
BEGIN
   c := GetData(d, mid) ;
   WITH c^ DO
      lower.doLook(d, ch, r) ;
      IF (r=allRight) OR (r=endOfLine)
      THEN
         ch := decryptChar(ch, key)
      END
   END
END dolook ;


PROCEDURE doskip (d: DeviceTablePtr) ;
VAR
   c: CipherInfo ;
BEGIN
   c := GetData(d, mid) ;
   WITH c^ DO
      lower.doSkip(d)
   END
END doskip ;


PROCEDURE doskiplook (d: DeviceTablePtr;
                      VAR ch: CHAR; VAR r: ReadResults) ;
VAR
   c: CipherInfo ;
BEGIN
   c := GetData(d, mid) ;
   WITH c^ DO
      lower.doSkipLook(d, ch, r) ;
      IF (r=allRight) OR (r=endOfLine)
      THEN
         ch := decryptChar(ch, key)
      END
   END
END doskiplook ;


PROCEDURE dowriteln (d: DeviceTablePtr) ;
VAR
   ch: CHAR ;
BEGIN
   ch := lf ;
   dotextwrite(d, ADR(ch), 1)
END dowriteln ;


PROCEDURE dotextread (d: DeviceTablePtr;
                      to: ADDRESS;
                      maxChars: CARDINAL;
                      VAR charsRead: CARDINAL) ;
VAR
   c : CipherInfo ;
   i : CARDINAL ;
   ch: CHAR ;
   p : PtrToChar ;
BEGIN
   c := GetData(d, mid) ;
   WITH c^ DO
      charsRead := 0 ;
      p := to ;
      WHILE charsRead<maxChars DO
         c^.lower.doTextRead(d, ADR(ch), SIZE(ch), i) ;
         IF i>0
         THEN
            p^ := decryptChar(ch, key) ;
            INC(p, SIZE(ch)) ;
            INC(charsRead, i)
         ELSE
            RETURN
         END
      END
   END
END dotextread ;


PROCEDURE dotextwrite (d: DeviceTablePtr;
                       from: ADDRESS;
                       charsToWrite: CARDINAL);
VAR
   c : CipherInfo ;
   i : CARDINAL ;
   ch: CHAR ;
   p : PtrToChar ;
BEGIN
   c := GetData(d, mid) ;
   WITH c^ DO
      p := from ;
      i := 0 ;
      WHILE i<charsToWrite DO
         ch := encryptChar(p^, key) ;
         c^.lower.doTextWrite(d, ADR(ch), SIZE(ch)) ;
         INC(p, SIZE(ch)) ;
         INC(i)
      END
   END
END dotextwrite ;


PROCEDURE dorawread (d: DeviceTablePtr;
                     to: ADDRESS;
                     maxLocs: CARDINAL;
                     VAR locsRead: CARDINAL) ;
VAR
   c: CipherInfo ;
   i: CARDINAL ;
   p: PtrToLoc ;
   l: LOC ;
BEGIN
   c := GetData(d, mid) ;
   WITH c^ DO
      locsRead := 0 ;
      p := to ;
      WHILE locsRead<maxLocs DO
         lower.doRawRead(d, ADR(l), SIZE(l), i) ;
         IF i>0
         THEN
            p^ := decryptLoc(d^.cid, d^.did, l, key) ;
            INC(p) ;
            INC(locsRead, i)
         ELSE
            RETURN
         END
      END
   END
END dorawread ;


PROCEDURE dorawwrite (d: DeviceTablePtr;
                      from: ADDRESS;
                      locsToWrite: CARDINAL) ;
VAR
   c: CipherInfo ;
   i: CARDINAL ;
   l: LOC ;
   p: PtrToLoc ;
BEGIN
   c := GetData(d, mid) ;
   WITH c^ DO
      p := from ;
      i := 0 ;
      WHILE i<locsToWrite DO
         l := encryptLoc(d^.cid, d^.did, p^, key) ;
         lower.doRawWrite(d, ADR(l), SIZE(l)) ;
         INC(p) ;
         INC(i)
      END
   END
END dorawwrite ;


PROCEDURE dogetname (d: DeviceTablePtr;
                     VAR a: ARRAY OF CHAR) ;
VAR
   c: CipherInfo ;
BEGIN
   c := GetData(d, mid) ;
   WITH c^ DO
      lower.doGetName(d, a) ;
      Insert('SimpleCipher (', 0, a) ;
      Append(')', a)
   END
END dogetname ;


(*
   freeData - disposes of, c.
*)

PROCEDURE freeData (c: CipherInfo) ;
BEGIN
   DISPOSE(c)
END freeData ;


(*
   dofree - replace original methods and then delete data pertaining
            to, mid.  The idea is that our new methods will call the
            old methods and then decrypt data when reading and visa
            versa for writing.  We write CHARs and LOCs at a time so
            ensure no plaintext data is ever passed outside this
            module.
*)

PROCEDURE dofree (d: DeviceTablePtr) ;
VAR
   c: CipherInfo ;
BEGIN
   c := GetData(d, mid) ;
   WITH d^ DO
      doLook := c^.lower.doLook ;
      doLook := c^.lower.doLook ;
      doSkip := c^.lower.doSkip ;
      doSkipLook := c^.lower.doSkipLook ;
      doLnWrite := c^.lower.doLnWrite ;
      doTextRead := c^.lower.doTextRead ;
      doTextRead := c^.lower.doTextRead ;
      doRawRead := c^.lower.doRawRead ;
      doRawWrite := c^.lower.doRawWrite ;
      doGetName := c^.lower.doGetName ;
      doReset := c^.lower.doReset ;
      doFlush := c^.lower.doFlush ;
      doFree := c^.lower.doFree
   END
END dofree ;


(*
   InsertCipherLayer - inserts a Caesar cipher below channel, cid.
                       The encryption, key, is specified.
*)

PROCEDURE InsertCipherLayer (cid: ChanId; key: INTEGER) ;
VAR
   did: DeviceId ;
   d  : DeviceTablePtr ;
   c  : CipherInfo ;
BEGIN
   did := GetDeviceId(cid) ;
   d := DeviceTablePtrValue(cid, did) ;
   IF GetData(d, mid)=NIL
   THEN
      NEW(c) ;
      c^.key := key ;
      c^.lower := d^ ;
      InitData(d, mid, c, freeData) ;
      WITH d^ DO
         doLook := dolook ;
         doSkip := doskip ;
         doSkipLook := doskiplook ;
         doLnWrite := dowriteln ;
         doTextRead := dotextread ;
         doTextWrite := dotextwrite ;
         doRawRead := dorawread ;
         doRawWrite := dorawwrite ;
         doGetName := dogetname ;
         (* doReset := doreset ; no need for either of these *)
         (* doFlush := doflush ; *)
         doFree := dofree
      END
   ELSE
      RAISEdevException(cid, did, notAvailable,
                        'SimpleCipher: unable to insert multiple cipher layers from the same module under a channel')
   END
END InsertCipherLayer ;


(*
   RemoveCipherLayer - removes a Caesar cipher below channel, cid.
*)

PROCEDURE RemoveCipherLayer (cid: ChanId) ;
VAR
   did: DeviceId ;
   d  : DeviceTablePtr ;
BEGIN
   did := GetDeviceId(cid) ;
   d := DeviceTablePtrValue(cid, did) ;
   IF GetData(d, mid)=NIL
   THEN
      RAISEdevException(cid, did, notAvailable,
                        'SimpleCipher: no cipher layer to remove from this channel')
   ELSE
      KillData(d, mid)
   END
END RemoveCipherLayer ;


BEGIN
   MakeModuleId(mid)
END SimpleCipher.
