(* ProgramArgs.mod implement the ISO ProgramArgs specification.

Copyright (C) 2008-2024 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE ProgramArgs ;

FROM RTgen IMPORT ChanDev, InitChanDev, DeviceType, doLook, doSkip, doSkipLook,
                  doReadText, doReadLocs ;

FROM SYSTEM IMPORT ADDRESS, ADR ;
FROM UnixArgs IMPORT GetArgC, GetArgV ;
FROM RTgenif IMPORT GenDevIF, InitGenDevIF ;
FROM RTdata IMPORT ModuleId, MakeModuleId, InitData, GetData ;
FROM IOLink IMPORT DeviceId, DeviceTablePtr, DeviceTablePtrValue, AllocateDeviceId, MakeChan, RAISEdevException ;
FROM IOChan IMPORT ChanExceptions ;
FROM IOConsts IMPORT ReadResults ;
FROM ChanConsts IMPORT read, text ;
FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM ASCII IMPORT nul, lf ;


TYPE
   PtrToChar = POINTER TO CHAR ;
   ArgInfo   = POINTER TO RECORD
                           currentPtr: PtrToChar ;
                           currentPos: CARDINAL ;
                           currentArg: CARDINAL ;
                           argLength : CARDINAL ;
                           argc      : CARDINAL ;
                        END ;


VAR
   mid      : ModuleId ;
   did      : DeviceId ;
   cid      : ChanId ;
   ArgData  : PtrToChar ;
   ArgLength: CARDINAL ;
   gen      : GenDevIF ;
   dev      : ChanDev ;


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


PROCEDURE textread (d: DeviceTablePtr;
                    to: ADDRESS;
                    maxChars: CARDINAL;
                    VAR charsRead: CARDINAL) ;
BEGIN
   doReadText(dev, d, to, maxChars, charsRead)
END textread ;


PROCEDURE rawread (d: DeviceTablePtr;
                   to: ADDRESS;
                   maxLocs: CARDINAL;
                   VAR locsRead: CARDINAL) ;
BEGIN
   doReadLocs(dev, d, to, maxLocs, locsRead)
END rawread ;


PROCEDURE getname (d: DeviceTablePtr;
                   VAR a: ARRAY OF CHAR) ;
BEGIN
   d^.doGetName(d, a)
END getname ;


PROCEDURE flush (d: DeviceTablePtr) ;
BEGIN
END flush ;


PROCEDURE handlefree (d: DeviceTablePtr) ;
BEGIN
END handlefree ;


PROCEDURE reset (d: DeviceTablePtr) ;
VAR
   a : ArgInfo ;
BEGIN
   a := GetData(d, mid) ;
   WITH a^ DO
      currentPtr := ArgData ;
      currentPos := 0 ;
      currentArg := 0 ;
      argLength := strlen(currentPtr)+1 ;
      argc := GetArgC ()
   END
END reset ;


(*
   doreadchar - returns a CHAR from the file associated with, g.
*)

PROCEDURE doreadchar (g: GenDevIF; d: DeviceTablePtr) : CHAR ;
VAR
   a : ArgInfo ;
   ch: CHAR ;
BEGIN
   d := DeviceTablePtrValue(cid, did) ;
   a := GetData(d, mid) ;
   WITH a^ DO
      IF currentPos<argLength
      THEN
         ch := currentPtr^ ;
         INC(currentPtr) ;
         INC(currentPos) ;
         d^.result := allRight ;
         RETURN( ch )
      ELSE
         d^.result := endOfInput ;
         RETURN( nul )
      END
   END
END doreadchar ;


(*
   dounreadchar - pushes a CHAR back onto the file associated with, g.
*)

PROCEDURE dounreadchar (g: GenDevIF; d: DeviceTablePtr; ch: CHAR) : CHAR ;
VAR
   a: ArgInfo ;
BEGIN
   d := DeviceTablePtrValue(cid, did) ;
   a := GetData(d, mid) ;
   WITH a^ DO
      IF currentPos>0
      THEN
         DEC(currentPtr) ;
         DEC(currentPos)
      END
   END ;
   RETURN( ch )
END dounreadchar ;


(*
   dogeterrno - returns the errno relating to the generic device.
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
   p: PtrToChar ;
   i: CARDINAL ;
BEGIN
   WITH d^ DO
      p := to ;
      i := 0 ;
      WHILE (i<max) AND ((result=notKnown) OR (result=allRight) OR (result=endOfLine)) DO
         p^ := doreadchar(g, d) ;
         INC(i) ;
         INC(p)
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
BEGIN
   RAISEdevException(cid, did, notAvailable,
                     'ProgramArgs.dowbytes:  not allowed to write to this channel') ;
   RETURN( FALSE )
END dowbytes ;


(*
   dowriteln - attempt to write an end of line marker to the
               file and returns TRUE if successful.
*)

PROCEDURE dowriteln (g: GenDevIF; d: DeviceTablePtr) : BOOLEAN ;
BEGIN
   RAISEdevException(cid, did, notAvailable,
                     'ProgramArgs.dowbytes:  not allowed to write to this channel') ;
   RETURN( FALSE )
END dowriteln ;


(*
   iseof - returns TRUE if end of file is seen.
*)

PROCEDURE iseof (g: GenDevIF; d: DeviceTablePtr) : BOOLEAN ;
VAR
   a: ArgInfo ;
BEGIN
   d := DeviceTablePtrValue(cid, did) ;
   a := GetData(d, mid) ;
   WITH a^ DO
      RETURN( currentPos=ArgLength )
   END
END iseof ;


(*
   iseoln - returns TRUE if end of line is seen.
*)

PROCEDURE iseoln (g: GenDevIF; d: DeviceTablePtr) : BOOLEAN ;
VAR
   ch: CHAR ;
BEGIN
   IF iseof(g, d)
   THEN
      RETURN( FALSE )
   ELSE
      ch := doreadchar(g, d) ;
      IF ch#dounreadchar(g, d, ch)
      THEN
         RAISEdevException(cid, did, hardDeviceError,
                           'ProgramArgs.iseoln:  internal inconsistancy error')
      END ;
      RETURN( ch=lf )
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
   strlen - returns the number characters in string at this point.
*)

PROCEDURE strlen (p: PtrToChar) : CARDINAL ;
VAR
   n: CARDINAL ;
BEGIN
   n := 0 ;
   WHILE p^#nul DO
      INC(n) ;
      INC(p)
   END ;
   RETURN( n )
END strlen ;


(*
   ArgChan - returns a value that identifies a channel for
             reading program arguments.
*)

PROCEDURE ArgChan () : ChanId ;
BEGIN
   RETURN( cid )
END ArgChan ;


(*
   IsArgPresent - tests if there is a current argument to
                  read from.  If not,
                  read <= IOChan.CurrentFlags() will be FALSE,
                  and attempting to read from the argument
                  channel will raise the exception
                  notAvailable.
*)

PROCEDURE IsArgPresent () : BOOLEAN ;
VAR
   d: DeviceTablePtr ;
   a: ArgInfo ;
BEGIN
   d := DeviceTablePtrValue(cid, did) ;
   a := GetData(d, mid) ;
   WITH a^ DO
      RETURN( currentArg<argc )
   END
END IsArgPresent ;


(*
   NextArg - if there is another argument, causes subsequent
             input from the argument device to come from the
             start of the next argument.  Otherwise there is
             no argument to read from, and a call of
             IsArgPresent will return FALSE.
*)
 
PROCEDURE NextArg ;
VAR
   d: DeviceTablePtr ;
   a: ArgInfo ;
   p: PtrToChar ;
BEGIN
   d := DeviceTablePtrValue(cid, did) ;
   a := GetData(d, mid) ;
   WITH a^ DO
      IF currentArg<argc
      THEN
         INC(currentArg) ;
         WHILE (currentPos<argLength) AND (currentPtr^#nul) DO
            INC(currentPos) ;
            INC(currentPtr)
         END ;
         INC(currentPtr) ;  (* move over nul onto first char of next arg *)
         argLength := strlen(currentPtr)+1 ;
         currentPos := 0
      END
   END
END NextArg ;


(*
   collectArgs - 
*)

PROCEDURE collectArgs ;
VAR
   i   : INTEGER ;
   n   : CARDINAL ;
   pp  : POINTER TO PtrToChar ;
   p, q: PtrToChar ;
BEGIN
   (* count the number of bytes necessary to remember all arg data *)
   n := 0 ;
   i := 0 ;
   pp := GetArgV () ;
   WHILE i < GetArgC () DO
      p := pp^ ;
      WHILE p^#nul DO
         INC(p) ;
         INC(n)
      END ;
      INC(n) ;
      INC(pp, SIZE(ADDRESS)) ;
      INC(i)
   END ;
   ArgLength := n ;
   (* now allocate correct amount of memory and copy the data *)
   ALLOCATE(ArgData, ArgLength) ;
   i := 0 ;
   pp := GetArgV () ;
   q := ArgData ;
   WHILE i < GetArgC () DO
      p := pp^ ;
      WHILE p^#nul DO
         q^ := p^ ;
         INC(q) ;
         INC(p)
      END ;
      q^ := p^ ;
      INC(q) ;
      INC(pp, SIZE(ADDRESS)) ;
      INC(i)
   END
END collectArgs ;


(*
   freeData - deallocates, a.
*)

PROCEDURE freeData (a: ArgInfo) ;
BEGIN
   DISPOSE(a)
END freeData ;


(*
   Init - 
*)

PROCEDURE Init ;
VAR
   d: DeviceTablePtr ;
   a: ArgInfo ;
BEGIN
   MakeModuleId(mid) ;
   AllocateDeviceId(did) ;
   MakeChan(did, cid) ;
   collectArgs ;
   NEW(a) ;
   WITH a^ DO
      currentPtr := ArgData ;
      currentPos := 0 ;
      currentArg := 0 ;
      argLength := strlen(currentPtr)+1 ;
      argc := GetArgC ()
   END ;
   d := DeviceTablePtrValue(cid, did) ;
   InitData(d, mid, a, freeData) ;
   gen := InitGenDevIF(did,
                       doreadchar, dounreadchar,
                       dogeterrno, dorbytes, dowbytes,
                       dowriteln,
                       iseof, iseoln, iserror) ;
   dev := InitChanDev(programargs, did, gen) ;
   WITH d^ DO
      flags := read + text ;
      errNum := 0 ;
      doLook := look ;
      doSkip := skip ;
      doSkipLook := skiplook ;
      doTextRead := textread ;
      doRawRead := rawread ;
      doGetName := getname ;
      doReset := reset ;
      doFlush := flush ;
      doFree := handlefree
   END
END Init ;


BEGIN
   Init
END ProgramArgs.
