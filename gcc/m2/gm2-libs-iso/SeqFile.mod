(* SeqFile.mod implement the ISO SeqFile specification.

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

IMPLEMENTATION MODULE SeqFile ;

FROM RTgen IMPORT ChanDev, DeviceType,
                  InitChanDev, doLook, doSkip, doSkipLook, doWriteLn,
                  doReadText, doWriteText, doReadLocs, doWriteLocs,
                  checkErrno ;

FROM RTfio IMPORT doreadchar, dounreadchar, dogeterrno, dorbytes,
                  dowbytes, dowriteln, iseof, iseoln, iserror ;

FROM IOLink IMPORT DeviceId, DeviceTablePtr, IsDevice, MakeChan, UnMakeChan,
                   DeviceTablePtrValue, RAISEdevException, AllocateDeviceId,
                   ResetProc ;

FROM RTgenif IMPORT GenDevIF, InitGenDevIF ;
FROM FIO IMPORT File ;
FROM errno IMPORT geterrno ;
FROM IOConsts IMPORT ReadResults ;
FROM ChanConsts IMPORT readFlag, writeFlag ;

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
                  VAR res: OpenResults;
                  toRead, toAppend: BOOLEAN;
                  whichreset: ResetProc) : ChanId ;
VAR
   c   : RTio.ChanId ;
   file: FIO.File ;
   e   : INTEGER ;
   p   : DeviceTablePtr ;
BEGIN
   IF toAppend
   THEN
      file := FIO.OpenForRandom (fname, NOT toRead, NOT FIO.Exists (fname))
   ELSIF toRead
   THEN
      file := FIO.OpenToRead (fname)
   ELSE
      file := FIO.OpenToWrite (fname)
   END ;
   checkOpenErrno (file, e, res) ;

   IF FIO.IsNoError (file)
   THEN
      MakeChan (did, c) ;
      RTio.SetFile (c, file) ;
      p := DeviceTablePtrValue (c, did) ;
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
         doReset := whichreset ;
         doFlush := flush ;
         doFree := handlefree
      END ;
      RETURN( c )
   ELSE
      RETURN( IOChan.InvalidChan () )
   END
END newCid ;


(*
   Attempts to obtain and open a channel connected to a stored rewindable
   file of the given name.  The write flag is implied; without the raw
   flag, text is implied.  If successful, assigns to cid the identity of
   the opened channel, assigns the value opened to res, and selects
   output mode, with the write position at the start of the file (i.e.
   the file is of zero length).  If a channel cannot be opened as required,
   the value of res indicates the reason, and cid identifies the
   invalid channel.
*)

PROCEDURE OpenWrite (VAR cid: ChanId; name: ARRAY OF CHAR; flags: FlagSet;
                     VAR res: OpenResults) ;
BEGIN
   INCL(flags, ChanConsts.writeFlag) ;
   IF NOT (ChanConsts.rawFlag IN flags)
   THEN
      INCL(flags, ChanConsts.textFlag)
   END ;
   cid := newCid(name, flags, res, FALSE, FALSE, resetWrite)
END OpenWrite ;


(*
   Attempts to obtain and open a channel connected to a stored rewindable
   file of the given name.  The read and old flags are implied; without
   the raw flag, text is implied.  If successful, assigns to cid the
   identity of the opened channel, assigns the value opened to res, and
   selects input mode, with the read position corresponding to the start
   of the file.  If a channel cannot be opened as required, the value of
   res indicates the reason, and cid identifies the invalid channel.
*)

PROCEDURE OpenRead (VAR cid: ChanId; name: ARRAY OF CHAR; flags: FlagSet;
                    VAR res: OpenResults) ;
BEGIN
   flags := flags + ChanConsts.read + ChanConsts.old ;
   IF NOT (ChanConsts.rawFlag IN flags)
   THEN
      INCL(flags, ChanConsts.textFlag)
   END ;
   cid := newCid(name, flags, res, TRUE, FALSE, resetRead)
END OpenRead ;


(*
   OpenAppend - attempts to obtain and open a channel connected
                to a stored rewindable file of the given name.
                The write and old flags are implied; without
                the raw flag, text is implied.  If successful,
                assigns to cid the identity of the opened channel,
                assigns the value opened to res, and selects output
                mode, with the write position corresponding to the
                length of the file.  If a channel cannot be opened
                as required, the value of res indicates the reason,
                and cid identifies the invalid channel.
*)

PROCEDURE OpenAppend (VAR cid: ChanId; name: ARRAY OF CHAR;
                      flags: FlagSet; VAR res: OpenResults) ;
BEGIN
   flags := flags + ChanConsts.write + ChanConsts.old ;
   IF NOT (ChanConsts.rawFlag IN flags)
   THEN
      INCL (flags, ChanConsts.textFlag)
   END ;
   cid := newCid (name, flags, res, FALSE, TRUE, resetAppend) ;
   IF IsSeqFile(cid)
   THEN
      FIO.SetPositionFromEnd (RTio.GetFile (cid), 0) ;
      checkErrno (dev, RTio.GetDevicePtr (cid))
   END
END OpenAppend ;


(*
   resetAppend - ensures that +write and -read and seeks to
                 the end of the file.
*)

PROCEDURE resetAppend (d: DeviceTablePtr) ;
VAR
   f: FIO.File ;
BEGIN
   WITH d^ DO
      flags := flags + write - read ;
      FIO.SetPositionFromEnd(RTio.GetFile(cid), 0) ;
   END ;
   checkErrno(dev, d)
END resetAppend ;


(*
   resetRead -
*)

PROCEDURE resetRead (d: DeviceTablePtr) ;
BEGIN
   Reread(d^.cid)
END resetRead ;


(*
   resetWrite -
*)

PROCEDURE resetWrite (d: DeviceTablePtr) ;
BEGIN
   Rewrite(d^.cid)
END resetWrite ;


(*
   IsSeqFile - tests if the channel identified by cid is open to a
               rewindable sequential file.
*)

PROCEDURE IsSeqFile (cid: ChanId) : BOOLEAN ;
BEGIN
   RETURN( (cid # NIL) AND (IOChan.InvalidChan() # cid) AND
           (IsDevice(cid, did)) AND
           ((ChanConsts.readFlag IN IOChan.CurrentFlags(cid)) OR
            (ChanConsts.writeFlag IN IOChan.CurrentFlags(cid))) )
END IsSeqFile ;


(*
   Reread - if the channel identified by cid is not open
            to a rewindable sequential file, the exception
            wrongDevice is raised; otherwise attempts to set
            the read position to the start of the file, and
            to select input mode.  If the operation cannot
            be performed (perhaps because of insufficient
            permissions) neither input mode nor output
            mode is selected.
*)

PROCEDURE Reread (cid: ChanId) ;
VAR
   d: DeviceTablePtr ;
BEGIN
   IF IsSeqFile(cid)
   THEN
      d := DeviceTablePtrValue(cid, did) ;
      WITH d^ DO
         EXCL(flags, writeFlag) ;
         IF readFlag IN flags
         THEN
            FIO.SetPositionFromBeginning(RTio.GetFile(cid), 0) ;
            checkErrno(dev, d)
         ELSE
            EXCL(flags, readFlag)
         END
      END
   ELSE
      RAISEdevException(cid, did, IOChan.wrongDevice,
                        'SeqFile.' + __FUNCTION__ +
                        ': channel is not a sequential file')
   END
END Reread ;


(*
   Rewrite - if the channel identified by cid is not open to a
             rewindable sequential file, the exception wrongDevice
             is raised; otherwise, attempts to truncate the
             file to zero length, and to select output mode.
             If the operation cannot be performed (perhaps
             because of insufficient permissions) neither input
             mode nor output mode is selected.
*)

PROCEDURE Rewrite (cid: ChanId) ;
VAR
   d: DeviceTablePtr ;
BEGIN
   IF IsSeqFile(cid)
   THEN
      d := DeviceTablePtrValue(cid, did) ;
      WITH d^ DO
         EXCL(flags, readFlag) ;
         IF writeFlag IN flags
         THEN
            FIO.SetPositionFromBeginning(RTio.GetFile(cid), 0) ;
            checkErrno(dev, d)
         ELSE
            EXCL(flags, writeFlag)
         END
      END
   ELSE
      RAISEdevException(cid, did, IOChan.wrongDevice,
                        'SeqFile.' + __FUNCTION__ +
                        ': channel is not a sequential file')
   END
END Rewrite ;


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
   IF IsSeqFile(cid)
   THEN
      UnMakeChan(did, cid) ;
      cid := IOChan.InvalidChan()
   ELSE
      RAISEdevException(cid, did, IOChan.wrongDevice,
                        'SeqFile.' + __FUNCTION__ +
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
END SeqFile.
