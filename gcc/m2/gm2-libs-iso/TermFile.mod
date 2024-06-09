(* TermFile.mod implement the ISO TermFile specification.

Copyright (C) 2009-2024 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE TermFile ;


FROM ASCII IMPORT nul, lf, cr ;
FROM ChanConsts IMPORT ChanFlags ;
FROM RTio IMPORT GetDeviceId ;
FROM RTgenif IMPORT GenDevIF, InitGenDevIF ;
FROM RTdata IMPORT ModuleId, MakeModuleId, InitData, GetData, KillData ;
FROM IOChan IMPORT ChanExceptions, InvalidChan, CurrentFlags ;
FROM IOConsts IMPORT ReadResults ;
FROM Strings IMPORT Assign ;

FROM IOLink IMPORT DeviceId, DeviceTable, DeviceTablePtr, DeviceTablePtrValue, IsDevice,
                   AllocateDeviceId, RAISEdevException, MakeChan, UnMakeChan ;

FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM Strings IMPORT Append ;


FROM SYSTEM IMPORT ADDRESS, ADR, LOC ;
FROM errno IMPORT geterrno ;
FROM ErrnoCategory IMPORT GetOpenResults ;

FROM RTgen IMPORT ChanDev, DeviceType, InitChanDev,
                  doLook, doSkip, doSkipLook, doWriteLn,
                  doReadText, doWriteText, doReadLocs, doWriteLocs,
                  checkErrno ;

FROM DynamicStrings IMPORT String, InitStringCharStar, CopyOut,
                           KillString ;

FROM termios IMPORT TERMIOS, InitTermios, KillTermios, tcgetattr,
                    tcsetattr, cfmakeraw, tcsnow ;


IMPORT libc ;


CONST
   O_RDONLY = 0 ;
   O_WRONLY = 1 ;

TYPE
   PtrToLoc   = POINTER TO LOC ;
   TermInfo   = POINTER TO RECORD
                              fd      : INTEGER ;
                              pushed  : CHAR ;
                              pushBack: BOOLEAN ;
                              old, new: TERMIOS ;
                           END ;

VAR
   mid: ModuleId ;
   did: DeviceId ;
   dev: ChanDev ;


(*
   InitTermInfo - creates a new TermInfo and initializes fields,
                  fd, and, pushed.
*)

PROCEDURE InitTermInfo (fd: INTEGER) : TermInfo ;
VAR
   t: TermInfo ;
BEGIN
   NEW(t) ;
   t^.fd := fd ;
   t^.pushBack := FALSE ;
   t^.new := InitTermios() ;
   t^.old := InitTermios() ;
   RETURN( t )
END InitTermInfo ;


(*
   KillTermInfo - deallocates memory associated with, t.
*)

PROCEDURE KillTermInfo (t: TermInfo) : TermInfo ;
BEGIN
   WITH t^ DO
      new := KillTermios(new) ;
      old := KillTermios(old)
   END ;
   DISPOSE(t) ;
   RETURN( NIL )
END KillTermInfo ;


(*
   getFd - return the file descriptor associated with, t.
*)

PROCEDURE getFd (t: TermInfo) : INTEGER ;
BEGIN
   RETURN( t^.fd )
END getFd ;


(*
   getPushBackChar - returns TRUE if a previously pushed back
                     character is available.  If TRUE then,
                     ch, will be assigned to the pushed back
                     character.
*)

PROCEDURE getPushBackChar (t: TermInfo; VAR ch: CHAR) : BOOLEAN ;
BEGIN
   WITH t^ DO
      IF pushBack
      THEN
         ch := pushed ;
         pushBack := FALSE ;
         RETURN( TRUE )
      ELSE
         RETURN( FALSE )
      END
   END
END getPushBackChar ;


(*
   setPushBackChar - attempts to push back, ch.  Only one character
                     may be pushed back consecutively.
*)

PROCEDURE setPushBackChar (t: TermInfo; ch: CHAR) : BOOLEAN ;
BEGIN
   WITH t^ DO
      IF pushBack
      THEN
         RETURN( FALSE )
      ELSE
         pushed := ch ;
         pushBack := TRUE ;
         RETURN( TRUE )
      END
   END
END setPushBackChar ;


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
   t : TermInfo ;
   ch: CHAR ;
BEGIN
   t := GetData(d, mid) ;
   WITH d^ DO
      fd := getFd(t) ;
      IF NOT getPushBackChar(t, ch)
      THEN
         REPEAT
            i := libc.read(fd, ADR(ch), SIZE(ch))
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
   t : TermInfo ;
BEGIN
   t := GetData(d, mid) ;
   WITH d^ DO
      fd := getFd(t) ;
      IF NOT setPushBackChar(t, ch)
      THEN
         RAISEdevException(cid, did, notAvailable,
                           'TermFile.dounreadchar: cannot push back more than one character consecutively')
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
   t : TermInfo ;
   p : PtrToLoc ;
   i : INTEGER ;
BEGIN
   t := GetData(d, mid) ;
   WITH d^ DO
      IF max>0
      THEN
         p := to ;
         IF getPushBackChar(t, p^)
         THEN
            actual := 1 ;
            RETURN( TRUE )
         END ;
         fd := getFd(t) ;
         i := libc.read(fd, p, max) ;
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
   END ;
   RETURN( FALSE )
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
   t : TermInfo ;
   i : INTEGER ;
BEGIN
   t := GetData(d, mid) ;
   WITH d^ DO
      fd := getFd(t) ;
      i := libc.write(fd, from, nBytes) ;
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


(*
   getname - assigns, a, to the device name of the terminal.
*)

PROCEDURE getname (d: DeviceTablePtr;
                   VAR a: ARRAY OF CHAR) ;
VAR
   s: String ;
BEGIN
   s := InitStringCharStar(libc.ttyname(0)) ;
   CopyOut(a, s) ;
   s := KillString(s)
END getname ;


(*
   freeData - disposes of, t.
*)

PROCEDURE freeData (t: TermInfo) ;
BEGIN
   t := KillTermInfo(t)
END freeData ;


(*
   handlefree -
*)

PROCEDURE handlefree (d: DeviceTablePtr) ;
VAR
   t : TermInfo ;
   fd: INTEGER ;
   i : INTEGER ;
BEGIN
   t := GetData(d, mid) ;
   fd := getFd(t) ;
   i := libc.close(fd) ;
   checkErrno(dev, d) ;
   KillData(d, mid)
END handlefree ;


(*
   termOpen - attempts to open up the terminal device.  It fills
              in any implied flags and returns a result depending
              whether the open was successful.
*)

PROCEDURE termOpen (t: TermInfo; VAR flagset: FlagSet; VAR e: INTEGER) : OpenResults ;
VAR
   i: INTEGER ;
BEGIN
   WITH t^ DO
      IF NOT (rawFlag IN flagset)
      THEN
         INCL(flagset, textFlag)
      END ;
      IF NOT (echoFlag IN flagset)
      THEN
         INCL(flagset, interactiveFlag)
      END ;
      IF NOT (writeFlag IN flagset)
      THEN
         INCL(flagset, readFlag)
      END ;
      IF writeFlag IN flagset
      THEN
         fd := libc.open(ADR("/dev/tty"), O_WRONLY, 0600B)
      ELSE
         fd := libc.open(ADR("/dev/tty"), O_RDONLY)
      END ;
      IF tcgetattr(fd, new)=0
      THEN
      END ;
      IF tcgetattr(fd, old)=0
      THEN
         IF rawFlag IN flagset
         THEN
            cfmakeraw(new)
         END ;
         IF tcsetattr(fd, tcsnow(), new)=0
         THEN
         END
      END ;
      e := geterrno() ;
      RETURN( GetOpenResults(e) )
   END
END termOpen ;


(*
   RestoreTerminalSettings -
*)

PROCEDURE RestoreTerminalSettings (cid: ChanId) ;
VAR
   d: DeviceTablePtr ;
   t: TermInfo ;
   e: INTEGER ;
BEGIN
   d := DeviceTablePtrValue(cid, did) ;
   t := GetData(d, mid) ;
   WITH t^ DO
      IF tcsetattr(fd, tcsnow(), old)=0
      THEN
      END
   END
END RestoreTerminalSettings ;


(*
   Open - attempts to obtain and open a channel connected to
          the terminal.  Without the raw flag, text is implied.
          Without the echo flag, line mode is requested,
          otherwise single character mode is requested.
          If successful, assigns to cid the identity of
          the opened channel, and assigns the value opened to res.
          If a channel cannot be opened as required, the value of
          res indicates the reason, and cid identifies the
          invalid channel.
*)

PROCEDURE Open (VAR cid: ChanId;
                flagset: FlagSet; VAR res: OpenResults) ;
VAR
   d: DeviceTablePtr ;
   t: TermInfo ;
   e: INTEGER ;
BEGIN
   MakeChan(did, cid) ;              (* create new channel *)
   d := DeviceTablePtrValue(cid, did) ;
   t := InitTermInfo(-1) ;
   res := termOpen(t, flagset, e) ;
   InitData(d, mid, t, freeData) ;   (* attach memory to device and module *)
   WITH d^ DO
      flags := flagset ;
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
END Open ;


(*
   IsTermFile - tests if the channel identified by cid is open to
                the terminal.
*)

PROCEDURE IsTermFile (cid: ChanId) : BOOLEAN ;
BEGIN
   RETURN( (cid # NIL) AND (InvalidChan() # cid) AND
           (IsDevice(cid, did)) AND
           ((readFlag IN CurrentFlags(cid)) OR
            (writeFlag IN CurrentFlags(cid))) )
END IsTermFile ;


(*
   Close - if the channel identified by cid is not open to the
           terminal, the exception wrongDevice is raised; otherwise
           closes the channel, and assigns the value identifying
           the invalid channel to cid.
*)

PROCEDURE Close (VAR cid: ChanId) ;
BEGIN
   IF IsTermFile(cid)
   THEN
      RestoreTerminalSettings(cid) ;
      UnMakeChan(did, cid) ;
      cid := InvalidChan()
   ELSE
      RAISEdevException(cid, did, wrongDevice,
                        'TermFile.' + __FUNCTION__ +
                        ': channel is opened to the terminal')
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
   AllocateDeviceId(did) ;
   gen := InitGenDevIF(did,
                       doreadchar, dounreadchar,
                       dogeterrno, dorbytes, dowbytes,
                       dowriteln,
                       iseof, iseoln, iserror) ;
   dev := InitChanDev(term, did, gen)
END Init ;


BEGIN
   Init
END TermFile.
