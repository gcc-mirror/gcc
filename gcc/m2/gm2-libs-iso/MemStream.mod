(* MemStream.mod provide a memory stream channel.

Copyright (C) 2015-2025 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE MemStream ;


FROM RTgen IMPORT ChanDev, DeviceType,
                  InitChanDev, doLook, doSkip, doSkipLook, doWriteLn,
                  doReadText, doWriteText, doReadLocs, doWriteLocs,
                  checkErrno ;

FROM RTdata IMPORT ModuleId, MakeModuleId, InitData, GetData, KillData ;

FROM IOLink IMPORT DeviceId, DeviceTablePtr, IsDevice, MakeChan, UnMakeChan,
                   DeviceTablePtrValue, RAISEdevException, AllocateDeviceId,
                   ResetProc ;

FROM Builtins IMPORT memcpy ;
FROM Assertion IMPORT Assert ;
FROM Strings IMPORT Assign ;
FROM RTgenif IMPORT GenDevIF, InitGenDevIF ;
FROM FIO IMPORT File ;
FROM IOConsts IMPORT ReadResults ;
FROM ChanConsts IMPORT readFlag, writeFlag ;
FROM SYSTEM IMPORT ADR ;
FROM ASCII IMPORT nl, nul ;
FROM Storage IMPORT ALLOCATE, DEALLOCATE, REALLOCATE ;
FROM libc IMPORT printf ;

IMPORT SYSTEM, RTio, errno, ErrnoCategory, ChanConsts, IOChan ;


CONST
   InitialLength = 128 ;
   Debugging     = FALSE ;

TYPE
   PtrToLoc      = POINTER TO LOC ;
   PtrToChar     = POINTER TO CHAR ;
   PtrToAddress  = POINTER TO ADDRESS ;
   PtrToCardinal = POINTER TO CARDINAL ;
   MemInfo       = POINTER TO RECORD
                                 buffer: ADDRESS ;
                                 length: CARDINAL ;
                                 index : CARDINAL ;
                                 pBuffer: PtrToAddress ;
                                 pLength: PtrToCardinal ;
                                 pUsed  : PtrToCardinal ;
                                 dealloc: BOOLEAN ;
                                 eof    : BOOLEAN ;
                                 eoln   : BOOLEAN ;
                              END ;

VAR
   dev: ChanDev ;
   did: DeviceId ;
   mid: ModuleId ;


(*
   Min -
*)

PROCEDURE Min (a, b: CARDINAL) : CARDINAL ;
BEGIN
   IF a<b
   THEN
      RETURN( a )
   ELSE
      RETURN( b )
   END
END Min ;


PROCEDURE look (d: DeviceTablePtr;
                VAR ch: CHAR; VAR r: ReadResults) ;
BEGIN
   doLook(dev, d, ch, r)
END look ;


PROCEDURE skip (d: DeviceTablePtr) ;
BEGIN
   doSkip(dev, d)
END skip ;


PROCEDURE skiplook (d: DeviceTablePtr;
                    VAR ch: CHAR; VAR r: ReadResults) ;
BEGIN
   doSkipLook(dev, d, ch, r)
END skiplook ;


PROCEDURE lnwrite (d: DeviceTablePtr) ;
BEGIN
   doWriteLn(dev, d)
END lnwrite ;


PROCEDURE textread (d: DeviceTablePtr;
                    to: SYSTEM.ADDRESS;
                    maxChars: CARDINAL;
                    VAR charsRead: CARDINAL) ;
BEGIN
   doReadText(dev, d, to, maxChars, charsRead)
END textread ;


PROCEDURE textwrite (d: DeviceTablePtr;
                     from: SYSTEM.ADDRESS;
                     charsToWrite: CARDINAL);
BEGIN
   doWriteText(dev, d, from, charsToWrite)
END textwrite ;


PROCEDURE rawread (d: DeviceTablePtr;
                   to: SYSTEM.ADDRESS;
                   maxLocs: CARDINAL;
                   VAR locsRead: CARDINAL) ;
BEGIN
   doReadLocs(dev, d, to, maxLocs, locsRead)
END rawread ;


PROCEDURE rawwrite (d: DeviceTablePtr;
                    from: SYSTEM.ADDRESS;
                    locsToWrite: CARDINAL) ;
BEGIN
   doWriteLocs(dev, d, from, locsToWrite)
END rawwrite ;


PROCEDURE getname (d: DeviceTablePtr;
                   VAR a: ARRAY OF CHAR) ;
BEGIN
   Assign('memstream', a)
END getname ;


PROCEDURE flush (d: DeviceTablePtr) ;
BEGIN
   (* nothing to do *)
END flush ;


(*
   doreadchar - returns a CHAR from the file associated with, g.
*)

PROCEDURE doreadchar (g: GenDevIF; d: DeviceTablePtr) : CHAR ;
VAR
   m : MemInfo ;
   pc: PtrToChar ;
BEGIN
   WITH d^ DO
      m := GetData(d, mid) ;
      WITH m^ DO
         IF index<length
         THEN
            pc := buffer ;
            INC(pc, index) ;
            INC(index) ;
            AssignIndex(m, index) ;
            eoln := (pc^=nl) ;
            eof := FALSE ;
            RETURN( pc^ )
         ELSE
            eof := TRUE ;
            eoln := FALSE ;
            RETURN( nul )
         END
      END
   END
END doreadchar ;


(*
   dounreadchar - pushes a CHAR back onto the file associated with, g.
*)

PROCEDURE dounreadchar (g: GenDevIF; d: DeviceTablePtr; ch: CHAR) : CHAR ;
VAR
   m : MemInfo ;
   pc: PtrToChar ;
BEGIN
   WITH d^ DO
      m := GetData(d, mid) ;
      WITH m^ DO
         IF index>0
         THEN
            DEC(index) ;
            AssignIndex(m, index) ;
            eof := FALSE ;
            pc := buffer ;
            INC(pc, index) ;
            eoln := (ch=nl) ;
            Assert(pc^=ch)    (* expecting to be pushing characters in exactly the reverse order *)
         ELSE
            Assert(FALSE) ;  (* expecting to be pushing characters in exactly the reverse order *)
         END
      END ;
      RETURN( ch )
   END
END dounreadchar ;


(*
   dogeterrno - always return 0 as the memstream device never invokes errno.
*)

PROCEDURE dogeterrno (g: GenDevIF; d: DeviceTablePtr) : INTEGER ;
BEGIN
   RETURN 0
END dogeterrno ;


(*
   dorbytes - reads upto, max, bytes setting, actual, and
              returning FALSE if an error (not due to eof)
              occurred.
*)

PROCEDURE dorbytes (g: GenDevIF; d: DeviceTablePtr;
                    to: ADDRESS;
                    max: CARDINAL;
                    VAR actual: CARDINAL) : BOOLEAN ;
VAR
   m : MemInfo ;
   pl: PtrToLoc ;
BEGIN
   WITH d^ DO
      m := GetData(d, mid) ;
      WITH m^ DO
         pl := buffer ;
         INC(pl, index) ;
         actual := Min(max, length-index) ;
         to := memcpy(to, pl, actual) ;
         INC(index, actual) ;
         AssignIndex(m, index) ;
         eof := FALSE ;
         eoln := FALSE
      END ;
      RETURN( TRUE )
   END
END dorbytes ;


(*
   dowbytes -
*)

PROCEDURE dowbytes (g: GenDevIF; d: DeviceTablePtr;
                    from: ADDRESS;
                    nBytes: CARDINAL;
                    VAR actual: CARDINAL) : BOOLEAN ;
VAR
   m : MemInfo ;
   pl: PtrToLoc ;
BEGIN
   WITH d^ DO
      m := GetData(d, mid) ;
      WITH m^ DO
         IF index+nBytes>length
         THEN
            WHILE index+nBytes>length DO
               (* buffer needs to grow *)
               length := length*2
            END ;
            REALLOCATE(buffer, length) ;
            AssignLength(m, length) ;
            AssignBuffer(m, buffer)
         END ;
         pl := buffer ;
         INC(pl, index) ;
         actual := Min(nBytes, length-index) ;
         pl := memcpy(pl, from, actual) ;
         INC(index, actual) ;
         AssignIndex(m, index)
      END ;
      RETURN( TRUE )
   END
END dowbytes ;


(*
   dowriteln - attempt to write an end of line marker to the
               file and returns TRUE if successful.
*)

PROCEDURE dowriteln (g: GenDevIF; d: DeviceTablePtr) : BOOLEAN ;
VAR
   ch: CHAR ;
   n : CARDINAL ;
BEGIN
   ch := nl ;
   RETURN( dowbytes(g, d, ADR(ch), SIZE(ch), n) )
END dowriteln ;


(*
   iseof - returns TRUE if end of file has been seen.
*)

PROCEDURE iseof (g: GenDevIF; d: DeviceTablePtr) : BOOLEAN ;
VAR
   m: MemInfo ;
BEGIN
   IF Debugging
   THEN
      printf ("mid = %p, d = %p\n", mid, d)
   END ;
   WITH d^ DO
      IF Debugging
      THEN
         printf ("mid = %p, d = %p\n", mid, d)
      END ;
      m := GetData(d, mid) ;
      RETURN( m^.eof )
   END
END iseof ;


(*
   iseoln - returns TRUE if end of line is seen.
*)

PROCEDURE iseoln (g: GenDevIF; d: DeviceTablePtr) : BOOLEAN ;
VAR
   m: MemInfo ;
BEGIN
   WITH d^ DO
      m := GetData(d, mid) ;
      RETURN( m^.eoln )
   END
END iseoln ;


(*
   iserror - returns TRUE if an error was seen on the device.
*)

PROCEDURE iserror (g: GenDevIF; d: DeviceTablePtr) : BOOLEAN ;
BEGIN
   RETURN( FALSE )
END iserror ;


(*
   AssignLength -
*)

PROCEDURE AssignLength (m: MemInfo; l: CARDINAL) ;
BEGIN
   WITH m^ DO
      length := l ;
      IF pLength#NIL
      THEN
         pLength^ := l
      END
   END
END AssignLength ;


(*
   AssignBuffer -
*)

PROCEDURE AssignBuffer (m: MemInfo; b: ADDRESS) ;
BEGIN
   WITH m^ DO
      buffer := b ;
      IF pBuffer#NIL
      THEN
         pBuffer^ := b
      END
   END
END AssignBuffer ;


(*
   AssignIndex -
*)

PROCEDURE AssignIndex (m: MemInfo; i: CARDINAL) ;
BEGIN
   WITH m^ DO
      index := i ;
      IF pUsed#NIL
      THEN
         pUsed^ := i
      END
   END
END AssignIndex ;


(*
   newCidWrite - returns a ChanId which represents the opened file, name.
                 res is set appropriately on return.
*)

PROCEDURE newCidWrite (f: FlagSet;
                       VAR res: OpenResults;
                       VAR buffer: ADDRESS;
                       VAR length: CARDINAL;
                       VAR used: CARDINAL;
                       deallocOnClose: BOOLEAN) : ChanId ;
VAR
   c: ChanId ;
   d: DeviceTablePtr ;
   m: MemInfo ;
BEGIN
   MakeChan(did, c) ;
   d := DeviceTablePtrValue(c, did) ;
   NEW(m) ;
   m^.pBuffer := ADR(buffer) ;
   m^.pLength := ADR(length) ;
   m^.pUsed := ADR(used) ;
   m^.dealloc := deallocOnClose ;
   ALLOCATE(m^.buffer, InitialLength) ;
   AssignBuffer(m, m^.buffer) ;
   AssignLength(m, InitialLength) ;
   AssignIndex(m, 0) ;
   InitData(d, mid, m, freeMemInfo) ;
   WITH d^ DO
      flags := f ;
      errNum := 0 ;
      doLook := look ;
      doSkip := skip ;
      doSkipLook := skiplook ;
      doLnWrite := lnwrite ;
      doTextRead := textread ;
      doTextWrite := textwrite ;
      doRawRead := rawread ;
      doRawWrite := rawwrite ;
      doGetName := getname ;
      doReset := resetWrite ;
      doFlush := flush ;
      doFree := handlefree
   END ;
   res := opened ;
   RETURN( c )
END newCidWrite ;


(*
   Attempts to obtain and open a channel connected to a contigeous
   buffer in memory.  The write flag is implied; without the raw
   flag, text is implied.  If successful, assigns to cid the identity of
   the opened channel, assigns the value opened to res.
   If a channel cannot be opened as required,
   the value of res indicates the reason, and cid identifies the
   invalid channel.

   The parameters, buffer, length and used maybe updated as
   data is written.  The buffer maybe reallocated
   and its address might alter, however the parameters will
   always reflect the current active buffer.  When this
   channel is closed the buffer is deallocated and
   buffer will be set to NIL, length and used will be set to
   zero.
*)

PROCEDURE OpenWrite (VAR cid: ChanId; flags: FlagSet;
                     VAR res: OpenResults;
                     VAR buffer: ADDRESS;
                     VAR length: CARDINAL;
                     VAR used: CARDINAL;
                     deallocOnClose: BOOLEAN) ;
BEGIN
   IF Debugging
   THEN
      printf ("OpenWrite called\n")
   END ;
   INCL(flags, ChanConsts.writeFlag) ;
   IF NOT (ChanConsts.rawFlag IN flags)
   THEN
      INCL(flags, ChanConsts.textFlag)
   END ;
   cid := newCidWrite(flags, res, buffer, length, used, deallocOnClose)
END OpenWrite ;


(*
   newCidRead - returns a ChanId which represents the opened file, name.
                res is set appropriately on return.
*)

PROCEDURE newCidRead (f: FlagSet;
                      VAR res: OpenResults;
                      buffer: ADDRESS;
                      length: CARDINAL;
                      deallocOnClose: BOOLEAN) : ChanId ;
VAR
   c: ChanId ;
   d: DeviceTablePtr ;
   m: MemInfo ;
BEGIN
   MakeChan(did, c) ;
   d := DeviceTablePtrValue(c, did) ;
   NEW(m) ;
   m^.pBuffer := NIL ;
   m^.pLength := NIL ;
   m^.pUsed := NIL ;
   m^.dealloc := deallocOnClose ;
   AssignBuffer(m, buffer) ;
   AssignLength(m, length) ;
   AssignIndex(m, 0) ;
   InitData(d, mid, m, freeMemInfo) ;
   WITH d^ DO
      flags := f ;
      errNum := 0 ;
      doLook := look ;
      doSkip := skip ;
      doSkipLook := skiplook ;
      doLnWrite := lnwrite ;
      doTextRead := textread ;
      doTextWrite := textwrite ;
      doRawRead := rawread ;
      doRawWrite := rawwrite ;
      doGetName := getname ;
      doReset := resetRead ;
      doFlush := flush ;
      doFree := handlefree
   END ;
   res := opened ;
   RETURN( c )
END newCidRead ;


(*
   freeMemInfo -
*)

PROCEDURE freeMemInfo (a: ADDRESS) ;
VAR
   m: MemInfo ;
BEGIN
   DEALLOCATE(a, SIZE(m^))
END freeMemInfo ;


(*
   Attempts to obtain and open a channel connected to a contigeous
   buffer in memory.  The read and old flags are implied; without
   the raw flag, text is implied.  If successful, assigns to cid the
   identity of the opened channel, assigns the value opened to res, and
   selects input mode, with the read position corresponding to the start
   of the buffer.  If a channel cannot be opened as required, the value of
   res indicates the reason, and cid identifies the invalid channel.
*)

PROCEDURE OpenRead (VAR cid: ChanId; flags: FlagSet;
                    VAR res: OpenResults;
                    buffer: ADDRESS; length: CARDINAL;
                    deallocOnClose: BOOLEAN) ;
BEGIN
   flags := flags + ChanConsts.read + ChanConsts.old ;
   IF NOT (ChanConsts.rawFlag IN flags)
   THEN
      INCL(flags, ChanConsts.textFlag)
   END ;
   cid := newCidRead(flags, res, buffer, length, deallocOnClose)
END OpenRead ;


(*
   resetRead - wrap a call to Reread.
*)

PROCEDURE resetRead (d: DeviceTablePtr) ;
BEGIN
   Reread(d^.cid)
END resetRead ;


(*
   resetWrite - wrap a call to Rewrite.
*)

PROCEDURE resetWrite (d: DeviceTablePtr) ;
BEGIN
   Rewrite(d^.cid)
END resetWrite ;


(*
   Reread - if the channel identified by cid is not open
            to a memory stream, the exception
            wrongDevice is raised; otherwise it sets the
            index to 0.  Subsequent reads will read the
            previous buffer contents.
*)

PROCEDURE Reread (cid: ChanId) ;
VAR
   d: DeviceTablePtr ;
   m: MemInfo ;
BEGIN
   IF IsMem(cid)
   THEN
      d := DeviceTablePtrValue(cid, did) ;
      WITH d^ DO
         EXCL(flags, writeFlag) ;
         IF readFlag IN flags
         THEN
            m := GetData(d, mid) ;
            AssignIndex(m, 0)
         ELSE
            EXCL(flags, readFlag)
         END
      END
   ELSE
      RAISEdevException(cid, did, IOChan.wrongDevice,
                        'MemStream.' + __FUNCTION__ +
                        ': channel is not a memory stream')
   END
END Reread ;


(*
   Rewrite - if the channel identified by cid is not open to a
             memory stream, the exception wrongDevice
             is raised; otherwise, it sets the index to 0.
             Subsequent writes will overwrite the previous buffer
             contents.
*)

PROCEDURE Rewrite (cid: ChanId) ;
VAR
   d: DeviceTablePtr ;
   m: MemInfo ;
BEGIN
   IF IsMem(cid)
   THEN
      d := DeviceTablePtrValue(cid, did) ;
      WITH d^ DO
         EXCL(flags, readFlag) ;
         IF writeFlag IN flags
         THEN
            m := GetData(d, mid) ;
            AssignIndex(m, 0)
         ELSE
            EXCL(flags, writeFlag)
         END
      END
   ELSE
      RAISEdevException(cid, did, IOChan.wrongDevice,
                        'MemStream.' + __FUNCTION__ +
                        ': channel is not a memory stream')
   END
END Rewrite ;


(*
   handlefree -
*)

PROCEDURE handlefree (d: DeviceTablePtr) ;
BEGIN
END handlefree ;


(*
   Close - if the channel identified by cid is not open to a sequential
           stream, the exception wrongDevice is raised; otherwise
           closes the channel, and assigns the value identifying
           the invalid channel to cid.
*)

PROCEDURE Close (VAR cid: ChanId) ;
BEGIN
   IF Debugging
   THEN
      printf ("Close called\n")
   END ;
   IF IsMem(cid)
   THEN
      UnMakeChan(did, cid) ;
      cid := IOChan.InvalidChan()
   ELSE
      RAISEdevException(cid, did, IOChan.wrongDevice,
                        'MemStream.' + __FUNCTION__ +
                        ': channel is not a sequential file')
   END
END Close ;


(*
   IsMem - tests if the channel identified by cid is open as
           a memory stream.
*)

PROCEDURE IsMem (cid: ChanId) : BOOLEAN ;
BEGIN
   RETURN( (cid # NIL) AND (IOChan.InvalidChan() # cid) AND
           (IsDevice(cid, did)) AND
           ((ChanConsts.readFlag IN IOChan.CurrentFlags(cid)) OR
            (ChanConsts.writeFlag IN IOChan.CurrentFlags(cid))) )
END IsMem ;


(*
   Init -
*)

PROCEDURE Init ;
VAR
   gen: GenDevIF ;
BEGIN
   MakeModuleId(mid) ;
   IF Debugging
   THEN
      printf ("mid = %d\n", mid)
   END ;
   AllocateDeviceId(did) ;
   gen := InitGenDevIF(did, doreadchar, dounreadchar,
                       dogeterrno, dorbytes, dowbytes,
                       dowriteln,
                       iseof, iseoln, iserror) ;
   dev := InitChanDev(streamfile, did, gen)
END Init ;


BEGIN
   Init
END MemStream.
