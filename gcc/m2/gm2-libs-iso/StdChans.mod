(* StdChans.mod implement the ISO StdChans specification.

Copyright (C) 2003-2023 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE StdChans ;

IMPORT FIO, IOLink, ChanConsts, SYSTEM, RTio ;

FROM RTio IMPORT SetFile, GetFile, GetDevicePtr ;
FROM IOConsts IMPORT ReadResults ;
FROM ChanConsts IMPORT read, write, text, raw, FlagSet ;
FROM RTgenif IMPORT GenDevIF, InitGenDevIF ;

FROM RTfio IMPORT doreadchar, dounreadchar,
                  dogeterrno, dorbytes, dowbytes,
                  dowriteln,
                  iseof, iseoln, iserror ;

FROM RTgen IMPORT ChanDev, DeviceType,
                  InitChanDev, doLook, doSkip, doSkipLook, doWriteLn,
                  doReadText, doWriteText, doReadLocs, doWriteLocs,
                  checkErrno ;


VAR
   in,
   out,
   err,
   stdin,
   stdout,
   stderr,
   stdnull: ChanId ;
   gen    : GenDevIF ;
   dev    : ChanDev ;
   did    : IOLink.DeviceId ;


PROCEDURE look (d: IOLink.DeviceTablePtr;
                VAR ch: CHAR; VAR r: ReadResults) ;
BEGIN
   doLook(dev, d, ch, r)
END look ;


PROCEDURE skip (d: IOLink.DeviceTablePtr) ;
BEGIN
   doSkip(dev, d)
END skip ;


PROCEDURE skiplook (d: IOLink.DeviceTablePtr;
                    VAR ch: CHAR; VAR r: ReadResults) ;
BEGIN
   doSkipLook(dev, d, ch, r)
END skiplook ;


PROCEDURE lnwrite (d: IOLink.DeviceTablePtr) ;
BEGIN
   doWriteLn(dev, d)
END lnwrite ;


PROCEDURE textread (d: IOLink.DeviceTablePtr;
                    to: SYSTEM.ADDRESS;
                    maxChars: CARDINAL;
                    VAR charsRead: CARDINAL) ;
BEGIN
   doReadText(dev, d, to, maxChars, charsRead)
END textread ;


PROCEDURE textwrite (d: IOLink.DeviceTablePtr;
                     from: SYSTEM.ADDRESS;
                     charsToWrite: CARDINAL);
BEGIN
   doWriteText(dev, d, from, charsToWrite)
END textwrite ;


PROCEDURE rawread (d: IOLink.DeviceTablePtr;
                   to: SYSTEM.ADDRESS;
                   maxLocs: CARDINAL;
                   VAR locsRead: CARDINAL) ;
BEGIN
   doReadLocs(dev, d, to, maxLocs, locsRead)
END rawread ;


PROCEDURE rawwrite (d: IOLink.DeviceTablePtr;
                    from: SYSTEM.ADDRESS;
                    locsToWrite: CARDINAL) ;
BEGIN
   doWriteLocs(dev, d, from, locsToWrite)
END rawwrite ;


PROCEDURE getname (d: IOLink.DeviceTablePtr;
                   VAR a: ARRAY OF CHAR) ;
BEGIN
   FIO.GetFileName(GetFile(d^.cid), a)
END getname ;


PROCEDURE flush (d: IOLink.DeviceTablePtr) ;
BEGIN
   FIO.FlushBuffer(GetFile(d^.cid))
END flush ;


PROCEDURE StdInChan () : ChanId ;
  (* Returns the identity of the implementation-defined standard source for
     program input.
  *)
BEGIN
   RETURN( stdin )
END StdInChan ;


PROCEDURE StdOutChan () : ChanId ;
  (* Returns the identity of the implementation-defined standard source for program
     output.
  *)
BEGIN
   RETURN( stdout )
END StdOutChan ;


PROCEDURE StdErrChan () : ChanId ;
  (* Returns the identity of the implementation-defined standard destination for program
     error messages.
  *)
BEGIN
   RETURN( stderr )
END StdErrChan ;


PROCEDURE NullChan () : ChanId ;
  (* Returns the identity of a channel open to the null device. *)
BEGIN
   RETURN( stdnull )
END NullChan ;


  (* The following functions return the default channel values *)

PROCEDURE InChan () : ChanId ;
  (* Returns the identity of the current default input channel. *)
BEGIN
   RETURN( in )
END InChan ;


PROCEDURE OutChan () : ChanId ;
  (* Returns the identity of the current default output channel. *)
BEGIN
   RETURN( out )
END OutChan ;


PROCEDURE ErrChan () : ChanId ;
  (* Returns the identity of the current default error message channel. *)
BEGIN
   RETURN( err )
END ErrChan ;

  (* The following procedures allow for redirection of the default channels *)

PROCEDURE SetInChan (cid: ChanId) ;
  (* Sets the current default input channel to that identified by cid. *)
BEGIN
   in := cid
END SetInChan ;


PROCEDURE SetOutChan (cid: ChanId) ;
  (* Sets the current default output channel to that identified by cid. *)
BEGIN
   out := cid
END SetOutChan ;


PROCEDURE SetErrChan (cid: ChanId) ;
  (* Sets the current default error channel to that identified by cid. *)
BEGIN
   err := cid
END SetErrChan ;


(*
   handlefree - 
*)

PROCEDURE handlefree (d: IOLink.DeviceTablePtr) ;
VAR
   f: FIO.File ;
BEGIN
   WITH d^ DO
      doFlush(d) ;
      checkErrno(dev, d) ;
      f := RTio.GetFile(RTio.ChanId(cid)) ;
      IF FIO.IsNoError(f)
      THEN
         FIO.FlushBuffer(f) ;
      END ;
      checkErrno(dev, d)
   END
END handlefree ;


(*
   SafeClose - only closes a channel if it was a StdChan.
*)

PROCEDURE SafeClose (VAR cid: ChanId) ;
BEGIN
   IF (cid#NIL) AND (cid#IOChan.InvalidChan()) AND IOLink.IsDevice(cid, did)
   THEN
      IOLink.UnMakeChan(did, cid) ;
      cid := IOChan.InvalidChan()
   END
END SafeClose ;


(*
   MapFile - 
*)

PROCEDURE MapFile (f: FIO.File; fl: ChanConsts.FlagSet) : IOChan.ChanId ;
VAR
   c: IOChan.ChanId ;
   d: IOLink.DeviceTablePtr ;
BEGIN
   IOLink.MakeChan(did, c) ;
   d := GetDevicePtr(c) ;
   WITH d^ DO
      result := notKnown ;
      SetFile(c, f) ;
      flags := fl ;
      doLook := look ;
      doSkip := skip ;
      doSkipLook := skiplook ;
      doLnWrite := lnwrite ;
      doTextRead := textread ;
      doTextWrite := textwrite ;
      doRawRead := rawread ;
      doRawWrite := rawwrite ;
      doGetName := getname ;
      (* doReset := reset ; *)
      doFlush := flush ;
      doFree := handlefree
   END ;
   RETURN( c )
END MapFile ;


(*
   Init - initializes the device and opens up the standard channels.
*)

PROCEDURE Init ;
BEGIN
   IOLink.AllocateDeviceId(did) ;
   IOLink.MakeChan(did, stdnull) ;

   gen := InitGenDevIF(did, doreadchar, dounreadchar,
                       dogeterrno, dorbytes, dowbytes,
                       dowriteln,
                       iseof, iseoln, iserror) ;
   dev := InitChanDev(stdchans, did, gen) ;

   stdin := MapFile(FIO.StdIn, read+text+raw) ;
   stdout := MapFile(FIO.StdOut, write+text+raw) ;
   stderr := MapFile(FIO.StdErr, write+text+raw) ;
   SetInChan(stdin) ;
   SetOutChan(stdout) ;
   SetErrChan(stderr) ;
END Init ;


BEGIN
   Init
FINALLY
   SafeClose(in) ;
   SafeClose(out) ;
   SafeClose(err) ;
   SafeClose(stdin) ;
   SafeClose(stdout) ;
   SafeClose(stderr)
END StdChans.
