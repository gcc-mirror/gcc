(* RTgen.mod implement a generic device interface used by ISO.

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

IMPLEMENTATION MODULE RTgen ;


FROM IOChan IMPORT ChanId, InvalidChan, ChanExceptions ;

FROM IOLink IMPORT DeviceTablePtrValue,
                   RAISEdevException ;

IMPORT ChanConsts ;
IMPORT IOConsts ;
IMPORT ErrnoCategory ;
IMPORT RTgen ;

FROM RTgenif IMPORT getDID,
                    doReadChar, doUnReadChar, doGetErrno,
                    doRBytes, doWBytes, doWrLn,
                    isEOF, isError, isEOLN ;

FROM ChanConsts IMPORT FlagSet, readFlag, writeFlag, rawFlag,
                       textFlag, read, write, raw, text ;

FROM Storage IMPORT ALLOCATE, DEALLOCATE ;


TYPE
   ChanDev = POINTER TO RECORD
                           type : DeviceType ;
                           did  : DeviceId ;
                           genif: GenDevIF ;
                        END ;

   deviceExceptions = ARRAY DeviceType OF BOOLEAN ;

VAR
   raiseEofInLook,
   raiseEofInSkip: deviceExceptions ;


(*
   InitChanDev - initialize and return a ChanDev.
*)

PROCEDURE InitChanDev (t: DeviceType; d: DeviceId; g: GenDevIF) : ChanDev ;
VAR
   c: ChanDev ;
BEGIN
   NEW(c) ;
   WITH c^ DO
      type := t ;
      did := d ;
      genif := g
   END ;
   RETURN( c )
END InitChanDev ;


(*
   KillChanDev - deallocates, g.
*)

PROCEDURE KillChanDev (g: GenDevIF) : GenDevIF ;
BEGIN
   DISPOSE(g) ;
   RETURN( NIL )
END KillChanDev ;


(* internal routine to check whether we have a valid channel *)

PROCEDURE checkValid (g: ChanDev; d: DeviceTablePtr) ;
BEGIN
   WITH d^ DO
      IF getDID(g^.genif)#did
      THEN
         RAISEdevException(cid, did, wrongDevice,
                           'operation attempted on an invalid channel')
      END ;
      IF (cid=InvalidChan()) OR (cid=NIL)
      THEN
         RAISEdevException(cid, did, wrongDevice,
                           'operation attempted on an invalid channel')
      END ;
      IF d#DeviceTablePtrValue(cid, did)
      THEN
         RAISEdevException(cid, did, wrongDevice,
                           'operation attempted on an invalid channel')
      END
   END
END checkValid ;


(*
   checkErrno - checks a number of errno conditions and raises
                appropriate ISO exceptions if they occur.
*)

PROCEDURE checkErrno (g: ChanDev; d: DeviceTablePtr) ;
BEGIN
   WITH d^ DO
      IF isError(g^.genif, d)
      THEN
         errNum := doGetErrno(g^.genif, d) ;
         IF ErrnoCategory.IsErrnoHard(errNum)
         THEN
            RAISEdevException(cid, did, notAvailable,
                              'unrecoverable (errno)')
         ELSIF ErrnoCategory.UnAvailable(errNum)
         THEN
            RAISEdevException(cid, did, notAvailable,
                              'unavailable (errno)')
         ELSIF errNum>0
         THEN
            RAISEdevException(cid, did, notAvailable,
                              'recoverable (errno)')
         END
      END
   END
END checkErrno ;


PROCEDURE checkPreRead (g: ChanDev;
                        d: DeviceTablePtr;
                        raise, raw: BOOLEAN) ;
BEGIN
   WITH d^ DO
      IF isEOF(g^.genif, d)
      THEN
         result := IOConsts.endOfInput ;
         IF raise
         THEN
            RAISEdevException(cid, did, skipAtEnd,
                              'attempting to read beyond end of file')
         END
      ELSIF (NOT raw) AND isEOLN(g^.genif, d)
      THEN
         result := IOConsts.endOfLine
      ELSE
         result := IOConsts.allRight
      END
   END
END checkPreRead ;


(*
   checkPostRead - checks whether an error occurred and sets
                   the result status.  This must only be called
                   after a read.
*)

PROCEDURE checkPostRead (g: ChanDev; d: DeviceTablePtr) ;
BEGIN
   checkErrno(g, d) ;
   setReadResult(g, d)
END checkPostRead ;


(*
   setReadResult -
*)

PROCEDURE setReadResult (g: ChanDev; d: DeviceTablePtr) ;
BEGIN
   WITH d^ DO
      IF isEOF(g^.genif, d)
      THEN
         result := IOConsts.endOfInput
      ELSIF isEOLN(g^.genif, d)
      THEN
         result := IOConsts.endOfLine
      ELSE
         result := IOConsts.allRight
      END
   END
END setReadResult ;


PROCEDURE checkPreWrite (g: ChanDev; d: DeviceTablePtr) ;
BEGIN
   (* nothing to do *)
END checkPreWrite ;


PROCEDURE checkPostWrite (g: ChanDev; d: DeviceTablePtr) ;
BEGIN
   checkErrno(g, d)
END checkPostWrite ;


(*
   checkFlags - checks read/write  raw/text consistancy flags.
*)

PROCEDURE checkFlags (f: FlagSet; d: DeviceTablePtr) ;
BEGIN
   WITH d^ DO
      IF (readFlag IN f) AND (NOT (readFlag IN flags))
      THEN
         RAISEdevException(cid, did, wrongDevice,
                           'attempting to read from a channel which was configured to write')
      END ;
      IF (writeFlag IN f) AND (NOT (writeFlag IN flags))
      THEN
         RAISEdevException(cid, did, wrongDevice,
                           'attempting to write to a channel which was configured to read')
      END ;
      IF (rawFlag IN f) AND (NOT (rawFlag IN flags))
      THEN
         IF readFlag IN flags
         THEN
            RAISEdevException(cid, did, notAvailable,
                              'attempting to read raw LOCs from a channel which was configured to read text')
         ELSE
            RAISEdevException(cid, did, notAvailable,
                              'attempting to write raw LOCs from a channel which was configured to write text')
         END
      END
   END
END checkFlags ;


(*
   RaiseEOFinLook - returns TRUE if the Look procedure
                    should raise an exception if it
                    sees end of file.
*)

PROCEDURE RaiseEOFinLook (g: ChanDev) : BOOLEAN ;
BEGIN
   RETURN( raiseEofInLook[g^.type] )
END RaiseEOFinLook ;


(*
   RaiseEOFinSkip - returns TRUE if the Skip procedure
                    should raise an exception if it
                    sees end of file.
*)

PROCEDURE RaiseEOFinSkip (g: ChanDev) : BOOLEAN ;
BEGIN
   RETURN( raiseEofInSkip[g^.type] )
END RaiseEOFinSkip ;


(*
   doLook - if there is a character as the next item in
            the input stream then it assigns its value
            to ch without removing it from the stream;
            otherwise the value of ch is not defined.
            r and result are set to the value allRight,
            endOfLine, or endOfInput.
*)

PROCEDURE doLook (g: ChanDev;
                  d: DeviceTablePtr;
                  VAR ch: CHAR;
                  VAR r: ReadResults) ;
BEGIN
   checkValid(g, d) ;
   WITH d^ DO
      checkErrno(g, d) ;
      checkPreRead(g, d, RaiseEOFinLook(g), ChanConsts.rawFlag IN flags) ;
      IF (result=IOConsts.allRight) OR (result=IOConsts.notKnown) OR
         (result=IOConsts.endOfLine)
      THEN
         ch := doReadChar(g^.genif, d) ;
         setReadResult(g, d) ;
         r := result ;
         ch := doUnReadChar(g^.genif, d, ch)
      END
   END
END doLook ;


(*
   doSkip -
*)

PROCEDURE doSkip (g: ChanDev;
                  d: DeviceTablePtr) ;
VAR
   ch: CHAR ;
BEGIN
   checkValid(g, d) ;
   WITH d^ DO
      checkPreRead(g, d, RaiseEOFinSkip(g), ChanConsts.rawFlag IN flags) ;
      ch := doReadChar(g^.genif, d) ;
      checkPostRead(g, d)
   END
END doSkip ;


(*
   doSkipLook - read a character, ignore it.  Read another and unread it
                return the new character.
*)

PROCEDURE doSkipLook (g: ChanDev;
                      d: DeviceTablePtr;
                      VAR ch: CHAR;
                      VAR r: ReadResults) ;
BEGIN
   doSkip(g, d) ;
   doLook(g, d, ch, r)
END doSkipLook ;


PROCEDURE doWriteLn (g: ChanDev;
                     d: DeviceTablePtr) ;
BEGIN
   checkValid(g, d) ;
   WITH d^ DO
      checkPreWrite(g, d) ;
      IF doWrLn(g^.genif, d)
      THEN
      END ;
      checkPostWrite(g, d)
   END
END doWriteLn ;


PROCEDURE doReadText (g: ChanDev;
                      d: DeviceTablePtr;
                      to: ADDRESS;
                      maxChars: CARDINAL;
                      VAR charsRead: CARDINAL) ;
VAR
   i: CARDINAL ;
BEGIN
   checkValid(g, d) ;
   checkFlags(read+text, d) ;
   IF maxChars>0
   THEN
      WITH d^ DO
         INCL(flags, textFlag) ;
         checkPreRead(g, d, FALSE, FALSE) ;
         charsRead := 0 ;
         REPEAT
            IF doRBytes(g^.genif, d, to, maxChars, i)
            THEN
               INC(charsRead, i) ;
               INC(to, i) ;
               DEC(maxChars, i)
            ELSE
               checkErrno(g, d) ;
               (* if our target system does not support errno then we *)
               RAISEdevException(cid, did, notAvailable,
                                 'textread unrecoverable errno')
            END
         UNTIL (maxChars=0) OR isEOF(g^.genif, d) ;
         checkPostRead(g, d)
      END
   END
END doReadText ;


PROCEDURE doWriteText (g: ChanDev;
                       d: DeviceTablePtr;
                       from: ADDRESS;
                       charsToWrite: CARDINAL) ;
VAR
   i: CARDINAL ;
BEGIN
   checkValid(g, d) ;
   checkFlags(write+text, d) ;
   WITH d^ DO
      checkPreWrite(g, d) ;
      INCL(flags, textFlag) ;
      WHILE (charsToWrite>0) AND doWBytes(g^.genif, d, from, charsToWrite, i) DO
         INC(from, i) ;
         DEC(charsToWrite, i)
      END ;
      IF isError(g^.genif, d)
      THEN
         checkErrno(g, d) ;
         (* if our target system does not support errno then we *)
         RAISEdevException(cid, did, notAvailable,
                           'textwrite unrecoverable errno')
      END ;
      checkPostWrite(g, d)
   END
END doWriteText ;


PROCEDURE doReadLocs (g: ChanDev;
                      d: DeviceTablePtr;
                      to: ADDRESS;
                      maxLocs: CARDINAL;
                      VAR locsRead: CARDINAL) ;
VAR
   i: CARDINAL ;
BEGIN
   checkValid(g, d) ;
   checkFlags(read+raw, d) ;
   IF maxLocs>0
   THEN
      WITH d^ DO
         INCL(flags, rawFlag) ;
         checkPreRead(g, d, FALSE, TRUE) ;
         locsRead := 0 ;
         REPEAT
            IF doRBytes(g^.genif, d, to, maxLocs, i)
            THEN
               INC(locsRead, i) ;
               INC(to, i) ;
               DEC(maxLocs, i)
            ELSE
               checkErrno(g, d) ;
               (* if our target system does not support errno then we *)
               RAISEdevException(cid, did, notAvailable,
                                 'rawread unrecoverable errno')
            END
         UNTIL (maxLocs=0) OR isEOF(g^.genif, d) ;
         checkPostRead(g, d)
      END
   END
END doReadLocs ;


PROCEDURE doWriteLocs (g: ChanDev;
                       d: DeviceTablePtr;
                       from: ADDRESS;
                       locsToWrite: CARDINAL) ;
VAR
   i: CARDINAL ;
BEGIN
   checkValid(g, d) ;
   checkFlags(write+raw, d) ;
   WITH d^ DO
      checkPreWrite(g, d) ;
      INCL(flags, rawFlag) ;
      WHILE doWBytes(g^.genif, d, from, locsToWrite, i) AND (i<locsToWrite) DO
         INC(from, i) ;
         DEC(locsToWrite, i)
      END ;
      IF isError(g^.genif, d)
      THEN
         checkErrno(g, d) ;
         (* if our target system does not support errno then we *)
         RAISEdevException(cid, did, notAvailable,
                           'rawwrite unrecoverable errno')
      END ;
      checkPostWrite(g, d)
   END
END doWriteLocs ;


BEGIN
   (*                                 seqfile, streamfile, programargs, stdchans, term , socket, rndfile *)
   raiseEofInLook := deviceExceptions{ FALSE  , FALSE     , FALSE      , FALSE   , FALSE, FALSE , FALSE };
   raiseEofInSkip := deviceExceptions{ TRUE   , TRUE      , TRUE       , TRUE    , TRUE , TRUE  , TRUE };
END RTgen.
