(* StreamFile.mod implement the ISO StreamFile specification.

Copyright (C) 2008-2023 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE StreamFile ;

FROM RTgen IMPORT ChanDev, DeviceType,
                  InitChanDev, doLook, doSkip, doSkipLook, doWriteLn,
                  doReadText, doWriteText, doReadLocs, doWriteLocs,
                  checkErrno ;

FROM RTfio IMPORT doreadchar, dounreadchar, dogeterrno, dorbytes,
                  dowbytes, dowriteln, iseof, iseoln, iserror ;

FROM IOLink IMPORT DeviceId, DeviceTablePtr, IsDevice, MakeChan, UnMakeChan,
                   DeviceTablePtrValue, RAISEdevException, AllocateDeviceId ;

FROM RTgenif IMPORT GenDevIF, InitGenDevIF ;
FROM FIO IMPORT File ;
FROM errno IMPORT geterrno ;
FROM IOConsts IMPORT ReadResults ;

IMPORT FIO, SYSTEM, RTio, errno, ErrnoCategory ;


VAR
   dev: ChanDev ;
   did: DeviceId ;


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
   FIO.GetFileName(RTio.GetFile(d^.cid), a)
END getname ;


PROCEDURE flush (d: DeviceTablePtr) ;
BEGIN
   FIO.FlushBuffer(RTio.GetFile(d^.cid))
END flush ;


(*
   checkOpenErrno - assigns, e, and, res, depending upon file result of opening,
                    file.
*)

PROCEDURE checkOpenErrno (file: FIO.File; VAR e: INTEGER; VAR res: OpenResults) ;
BEGIN
   IF FIO.IsNoError(file)
   THEN
      e := 0 ;
   ELSE
      e := errno.geterrno()
   END ;
   res := ErrnoCategory.GetOpenResults(e)
END checkOpenErrno ;


(*
   newCid - returns a ChanId which represents the opened file, name.
            res is set appropriately on return.
*)

PROCEDURE newCid (fname: ARRAY OF CHAR;
                  f: FlagSet;
                  VAR res: OpenResults) : ChanId ;
VAR
   c   : RTio.ChanId ;
   file: FIO.File ;
   e   : INTEGER ;
   p   : DeviceTablePtr ;
BEGIN
   IF ChanConsts.readFlag IN f
   THEN
      file := FIO.OpenToRead(fname)
   ELSE
      file := FIO.OpenToWrite(fname)
   END ;
   checkOpenErrno(file, e, res) ;

   IF FIO.IsNoError(file)
   THEN
      MakeChan(did, c) ;
      RTio.SetFile(c, file) ;
      p := DeviceTablePtrValue(c, did) ;
      WITH p^ DO
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
         (* doReset := reset ; *)
         doFlush := flush ;
         doFree := handlefree
      END ;
      RETURN( c )
   ELSE
      RETURN( IOChan.InvalidChan() )
   END
END newCid ;


(*
   Open - attempts to obtain and open a channel connected to a
          sequential stream of the given name.
          The read flag implies old; without the raw flag,
          text is implied.  If successful, assigns to cid
          the identity of the opened channel, and assigns the
          value opened to res.  If a channel cannot be opened
          as required, the value of res indicates the reason,
          and cid identifies the invalid channel.
*)

PROCEDURE Open (VAR cid: ChanId; name: ARRAY OF CHAR;
                flags: FlagSet; VAR res: OpenResults) ;
BEGIN
   IF NOT (ChanConsts.rawFlag IN flags)
   THEN
      INCL(flags, ChanConsts.textFlag)
   END ;
   cid := newCid(name, flags, res)
END Open ;


(*
   IsStreamFile - tests if the channel identified by cid is
                  open to a sequential stream.
*)

PROCEDURE IsStreamFile (cid: ChanId) : BOOLEAN ;
BEGIN
   RETURN( (cid # NIL) AND (IOChan.InvalidChan() # cid) AND
           (IsDevice(cid, did)) AND
           ((ChanConsts.readFlag IN IOChan.CurrentFlags(cid)) OR
            (ChanConsts.writeFlag IN IOChan.CurrentFlags(cid))) )
END IsStreamFile ;


(*
   handlefree - 
*)

PROCEDURE handlefree (d: DeviceTablePtr) ;
VAR
   f: File ;
BEGIN
   WITH d^ DO
      doFlush(d) ;
      checkErrno(dev, d) ;
      f := RTio.GetFile(RTio.ChanId(cid)) ;
      IF FIO.IsNoError(f)
      THEN
         FIO.Close(f) ;
      END ;
      checkErrno(dev, d)
   END
END handlefree ;


(*
   Close - if the channel identified by cid is not open to a sequential
           stream, the exception wrongDevice is raised; otherwise
           closes the channel, and assigns the value identifying
           the invalid channel to cid.
*)

PROCEDURE Close (VAR cid: ChanId) ;
BEGIN
   IF IsStreamFile(cid)
   THEN
      UnMakeChan(did, cid) ;
      cid := IOChan.InvalidChan()
   ELSE
      RAISEdevException(cid, did, IOChan.wrongDevice,
                        'StreamFile.' + __FUNCTION__ +
                        ': channel is not a sequential file')
   END
END Close ;


(*
   Init - 
*)

PROCEDURE Init ;
VAR
   gen: GenDevIF ;
BEGIN
   AllocateDeviceId(did) ;
   gen := InitGenDevIF(did, doreadchar, dounreadchar,
                       dogeterrno, dorbytes, dowbytes,
                       dowriteln,
                       iseof, iseoln, iserror) ;
   dev := InitChanDev(streamfile, did, gen)
END Init ;


BEGIN
   Init
END StreamFile.
