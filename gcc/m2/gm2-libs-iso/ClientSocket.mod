(* ClientSocket.mod provides a client TCP interface for ChanId's.

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

IMPLEMENTATION MODULE ClientSocket ;


FROM ASCII IMPORT nul, lf, cr ;
FROM ChanConsts IMPORT ChanFlags ;
FROM RTio IMPORT GetDeviceId ;
FROM RTgenif IMPORT GenDevIF, InitGenDevIF ;
FROM RTdata IMPORT ModuleId, MakeModuleId, InitData, GetData, KillData ;
FROM IOChan IMPORT ChanExceptions, InvalidChan, CurrentFlags ;
FROM IOConsts IMPORT ReadResults ;

FROM IOLink IMPORT DeviceId, DeviceTable, DeviceTablePtr, DeviceTablePtrValue, IsDevice,
                   AllocateDeviceId, RAISEdevException, MakeChan, UnMakeChan ;

FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM Strings IMPORT Append ;
FROM SYSTEM IMPORT ADDRESS, ADR, LOC ;
FROM libc IMPORT read, write, close ;
FROM errno IMPORT geterrno ;
FROM ErrnoCategory IMPORT GetOpenResults ;
FROM WholeStr IMPORT IntToStr ;

FROM RTgen IMPORT ChanDev, DeviceType, InitChanDev,
                  doLook, doSkip, doSkipLook, doWriteLn,
                  doReadText, doWriteText, doReadLocs, doWriteLocs,
                  checkErrno ;

FROM wrapsock IMPORT clientInfo, clientOpen, clientOpenIP, getClientPortNo,
                     getClientSocketFd, getClientIP, getSizeOfClientInfo,
                     getPushBackChar, setPushBackChar, getClientHostname ;


TYPE
   PtrToLoc   = POINTER TO LOC ;
   ClientInfo = ADDRESS ;
VAR
   mid           : ModuleId ;
   did           : DeviceId ;
   dev           : ChanDev ;
   ClientInfoSize: CARDINAL ;


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
                    to: ADDRESS;
                    maxChars: CARDINAL;
                    VAR charsRead: CARDINAL) ;
BEGIN
   doReadText(dev, d, to, maxChars, charsRead)
END textread ;


PROCEDURE textwrite (d: DeviceTablePtr;
                     from: ADDRESS;
                     charsToWrite: CARDINAL);
BEGIN
   doWriteText(dev, d, from, charsToWrite)
END textwrite ;


PROCEDURE rawread (d: DeviceTablePtr;
                   to: ADDRESS;
                   maxLocs: CARDINAL;
                   VAR locsRead: CARDINAL) ;
BEGIN
   doReadLocs(dev, d, to, maxLocs, locsRead)
END rawread ;


PROCEDURE rawwrite (d: DeviceTablePtr;
                    from: ADDRESS;
                    locsToWrite: CARDINAL) ;
BEGIN
   doWriteLocs(dev, d, from, locsToWrite)
END rawwrite ;


(*
   doreadchar - returns a CHAR from the file associated with, g.
*)

PROCEDURE doreadchar (g: GenDevIF; d: DeviceTablePtr) : CHAR ;
VAR
   i : INTEGER ;
   fd: INTEGER ;
   c : ClientInfo ;
   ch: CHAR ;
BEGIN
   c := GetData(d, mid) ;
   WITH d^ DO
      fd := getClientSocketFd(c) ;
      IF NOT getPushBackChar(c, ch)
      THEN
         REPEAT
            i := read(fd, ADR(ch), SIZE(ch))
         UNTIL i#0 ;
         IF i<0
         THEN
            errNum := geterrno()
         END
      END ;
      RETURN( ch )
   END
END doreadchar ;


(*
   dounreadchar - pushes a CHAR back onto the file associated with, g.
*)

PROCEDURE dounreadchar (g: GenDevIF; d: DeviceTablePtr; ch: CHAR) : CHAR ;
VAR
   fd: INTEGER ;
   c : ClientInfo ;
BEGIN
   c := GetData(d, mid) ;
   WITH d^ DO
      fd := getClientSocketFd(c) ;
      IF NOT setPushBackChar(c, ch)
      THEN
         RAISEdevException(cid, did, notAvailable,
                           'ClientSocket.dounreadchar: number of characters pushed back exceeds buffer')
      END ;
      RETURN( ch )
   END
END dounreadchar ;


(*
   dogeterrno - returns the errno relating to the generic device.
*)

PROCEDURE dogeterrno (g: GenDevIF; d: DeviceTablePtr) : INTEGER ;
BEGIN
   RETURN geterrno()
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
   fd: INTEGER ;
   c : ClientInfo ;
   p : PtrToLoc ;
   i : INTEGER ;
BEGIN
   c := GetData(d, mid) ;
   WITH d^ DO
      IF max>0
      THEN
         p := to ;
         IF getPushBackChar(c, p^)
         THEN
            actual := 1 ;
            RETURN( TRUE )
         END ;
         fd := getClientSocketFd(c) ;
         i := read(fd, p, max) ;
         IF i>=0
         THEN
            actual := i ;
            RETURN( TRUE )
         ELSE
            errNum := geterrno() ;
            actual := 0 ;
            RETURN( FALSE )
         END
      ELSE
         RETURN( FALSE )
      END
   END
END dorbytes ;


(*
   dowbytes - attempts to write out nBytes.  The actual
              number of bytes written are returned.
              If the actual number of bytes written is >= 0 then
              the return result will be true.  Failure to
              write any bytes results in returning FALSE
              errno set and the actual will be set to zero.
*)

PROCEDURE dowbytes (g: GenDevIF; d: DeviceTablePtr;
                    from: ADDRESS;
                    nBytes: CARDINAL;
                    VAR actual: CARDINAL) : BOOLEAN ;
VAR
   fd: INTEGER ;
   c : ClientInfo ;
   i : INTEGER ;
BEGIN
   c := GetData(d, mid) ;
   WITH d^ DO
      fd := getClientSocketFd(c) ;
      i := write(fd, from, nBytes) ;
      IF i>=0
      THEN
         actual := i ;
         RETURN( TRUE )
      ELSE
         errNum := geterrno() ;
         actual := 0 ;
         RETURN( FALSE )
      END
   END
END dowbytes ;


(*
   dowriteln - attempt to write an end of line marker to the
               file and returns TRUE if successful.
*)

PROCEDURE dowriteln (g: GenDevIF; d: DeviceTablePtr) : BOOLEAN ;
VAR
   a: ARRAY [0..1] OF CHAR ;
   i: CARDINAL ;
BEGIN
   a[0] := cr ;
   a[1] := lf ;
   RETURN( dowbytes(g, d, ADR(a), SIZE(a), i) AND (i=SIZE(a)) )
END dowriteln ;


(*
   iseof - returns TRUE if end of file is seen.
*)

PROCEDURE iseof (g: GenDevIF; d: DeviceTablePtr) : BOOLEAN ;
VAR
   ch: CHAR ;
BEGIN
   ch := doreadchar(g, d) ;
   WITH d^ DO
      IF errNum=0
      THEN
         ch := dounreadchar(g, d, ch) ;
         RETURN( FALSE )
      ELSE
         RETURN( TRUE )
      END
   END
END iseof ;


(*
   iseoln - returns TRUE if end of line is seen.
*)

PROCEDURE iseoln (g: GenDevIF; d: DeviceTablePtr) : BOOLEAN ;
VAR
   ch: CHAR ;
BEGIN
   ch := doreadchar(g, d) ;
   WITH d^ DO
      IF errNum=0
      THEN
         ch := dounreadchar(g, d, ch) ;
         RETURN( ch=lf )
      ELSE
         RETURN( FALSE )
      END
   END
END iseoln ;


(*
   iserror - returns TRUE if an error was seen on the device.
*)

PROCEDURE iserror (g: GenDevIF; d: DeviceTablePtr) : BOOLEAN ;
BEGIN
   RETURN( d^.errNum#0 )
END iserror ;


PROCEDURE getname (d: DeviceTablePtr;
                   VAR a: ARRAY OF CHAR) ;
VAR
   c: ClientInfo ;
   b: ARRAY [0..6] OF CHAR ;
BEGIN
   c := GetData(d, mid) ;
   getClientHostname(c, ADR(a), HIGH(a)) ;
   Append(':', a) ;
   IntToStr(getClientPortNo(c) , b) ;
   Append(b, a)
END getname ;


(*
   freeData - disposes of, c.
*)

PROCEDURE freeData (c: ClientInfo) ;
BEGIN
   DEALLOCATE(c, ClientInfoSize) ;
END freeData ;


(*
   handlefree -
*)

PROCEDURE handlefree (d: DeviceTablePtr) ;
VAR
   c : ClientInfo ;
   fd: INTEGER ;
   i : INTEGER ;
BEGIN
   c := GetData(d, mid) ;
   fd := getClientSocketFd(c) ;
   i := close(fd) ;
   checkErrno(dev, d) ;
   KillData(d, mid)
END handlefree ;


(*
   OpenSocket - opens a TCP client connection to host:port.
*)

PROCEDURE OpenSocket (VAR cid: ChanId;
                      host: ARRAY OF CHAR; port: CARDINAL;
                      f: FlagSet; VAR res: OpenResults) ;
VAR
   d: DeviceTablePtr ;
   c: ClientInfo ;
   e: INTEGER ;
BEGIN
   MakeChan(did, cid) ;              (* create new channel *)
   ALLOCATE(c, ClientInfoSize) ;     (* allocate client socket memory *)
   d := DeviceTablePtrValue(cid, did) ;
   InitData(d, mid, c, freeData) ;   (* attach memory to device and module *)
   res := clientOpen(c, ADR(host), LENGTH(host), port) ;
   IF res=opened
   THEN
      e := 0
   ELSE
      e := geterrno()
   END ;
   WITH d^ DO
      flags := f ;
      errNum := e ;
      doLook := look ;
      doSkip := skip ;
      doSkipLook := skiplook ;
      doLnWrite := lnwrite ;
      doTextRead := textread ;
      doTextWrite := textwrite ;
      doRawRead := rawread ;
      doRawWrite := rawwrite ;
      doGetName := getname ;
      doFree := handlefree
   END
END OpenSocket ;


(*
   IsSocket - tests if the channel identified by cid is open as
              a client socket stream.
*)

PROCEDURE IsSocket (cid: ChanId) : BOOLEAN ;
BEGIN
   RETURN( (cid # NIL) AND (InvalidChan() # cid) AND
           (IsDevice(cid, did)) AND
           ((readFlag IN CurrentFlags(cid)) OR
            (writeFlag IN CurrentFlags(cid))) )
END IsSocket ;


(*
   Close - if the channel identified by cid is not open to a socket
           stream, the exception wrongDevice is raised; otherwise
           closes the channel, and assigns the value identifying
           the invalid channel to cid.
*)

PROCEDURE Close (VAR cid: ChanId) ;
BEGIN
   IF IsSocket(cid)
   THEN
      UnMakeChan(did, cid) ;
      cid := InvalidChan()
   ELSE
      RAISEdevException(cid, did, wrongDevice,
                        'ClientSocket.' + __FUNCTION__ +
                        ': channel is not a socket stream')
   END
END Close ;


(*
   Init -
*)

PROCEDURE Init ;
VAR
   gen: GenDevIF ;
BEGIN
   MakeModuleId(mid) ;
   ClientInfoSize := getSizeOfClientInfo() ;
   AllocateDeviceId(did) ;
   gen := InitGenDevIF(did, doreadchar, dounreadchar,
                       dogeterrno, dorbytes, dowbytes,
                       dowriteln,
                       iseof, iseoln, iserror) ;
   dev := InitChanDev(streamfile, did, gen)
END Init ;


BEGIN
   Init
END ClientSocket.
