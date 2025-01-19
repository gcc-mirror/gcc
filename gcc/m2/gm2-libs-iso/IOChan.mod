(* IOChan.mod implement the ISO IOChan specification.

Copyright (C) 2002-2025 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE IOChan ;

IMPORT FIO, EXCEPTIONS, M2EXCEPTION, RTio,
       RTentity, errno, ErrnoCategory, IOLink, StdChans, M2RTS ;

FROM EXCEPTIONS IMPORT ExceptionSource, RAISE, AllocateSource,
                       IsCurrentSource, IsExceptionalExecution ;
FROM Storage IMPORT ALLOCATE ;


TYPE
   ChanId = RTio.ChanId ;

VAR
   iochan :  ExceptionSource ;
   invalid:  ChanId ;


PROCEDURE InvalidChan () : ChanId ;
  (* Returns the value identifying the invalid channel. *)
BEGIN
   RETURN( invalid )
END InvalidChan ;


PROCEDURE CheckValid (cid: ChanId) ;
  (* internal routine to check whether we have a valid channel *)
BEGIN
   IF cid=InvalidChan()
   THEN
      RAISE(iochan, ORD(notAChannel), 'IOChan: ChanId specified is invalid')
   END
END CheckValid ;


  (* For each of the following operations, if the device supports the
     operation on the channel, the behaviour of the procedure conforms
     with the description below.  The full behaviour is defined for
     each device module.  If the device does not support the operation
     on the channel, the behaviour of the procedure is to raise the
     exception notAvailable.
  *)

  (* Text operations - these perform any required translation
     between the internal and external representation of text.
  *)

PROCEDURE Look (cid: ChanId; VAR ch: CHAR; VAR res: IOConsts.ReadResults) ;
  (* If there is a character as the next item in the input stream cid,
     assigns its value to ch without removing it from the stream;
     otherwise the value of ch is not defined.
     res (and the stored read result) are set to the value
     allRight, endOfLine, or endOfInput.
  *)
VAR
   did: IOLink.DeviceId ;
   dtp: IOLink.DeviceTablePtr ;
BEGIN
   CheckValid(cid) ;
   did := RTio.GetDeviceId(cid) ;
   dtp := IOLink.DeviceTablePtrValue(cid, did) ;
   IF dtp=NIL
   THEN
      RAISE(iochan, ORD(hardDeviceError),
            'IOChan.Look: device table ptr is NIL')
   ELSE
      WITH dtp^ DO
         IF cid=StdChans.NullChan()
         THEN
            result := IOConsts.endOfInput
         ELSIF (ChanConsts.readFlag IN flags) AND (ChanConsts.textFlag IN flags)
         THEN
            doLook(dtp, ch, res)
         ELSE
            res := IOConsts.wrongFormat
         END ;
         result := res
      END
   END
END Look ;


PROCEDURE Skip (cid: ChanId) ;
  (* If the input stream cid has ended, the exception skipAtEnd is raised;
     otherwise the next character or line mark in cid is removed,
     and the stored read result is set to the value allRight.
  *)
VAR
   did: IOLink.DeviceId ;
   dtp: IOLink.DeviceTablePtr ;
BEGIN
   CheckValid(cid) ;
   did := RTio.GetDeviceId(cid) ;
   dtp := IOLink.DeviceTablePtrValue(cid, did) ;
   IF dtp=NIL
   THEN
      RAISE(iochan, ORD(hardDeviceError),
            'IOChan.Skip: device table ptr is NIL')
   ELSE
      WITH dtp^ DO
         IF (cid=StdChans.NullChan()) OR (result=IOConsts.endOfInput)
         THEN
            RAISE(iochan, ORD(skipAtEnd),
                  'IOChan.Skip: attempt to skip data from a stream that has ended')
         ELSIF (ChanConsts.readFlag IN flags) AND (ChanConsts.textFlag IN flags)
         THEN
            doSkip(dtp) ;
            result := IOConsts.allRight
         ELSE
            RAISE(iochan, ORD(notAvailable),
                  'IOChan.Skip: attempt to skip data from a channel which is not configured as read and text')
         END
      END
   END
END Skip ;


PROCEDURE SkipLook (cid: ChanId;
                    VAR ch: CHAR;
                    VAR res: IOConsts.ReadResults) ;
  (* If the input stream cid has ended, the exception skipAtEnd is raised;
     otherwise the next character or line mark in cid is removed.
     If there is a character as the next item in cid stream,
     assigns its value to ch without removing it from the stream.
     Otherwise, the value of ch is not defined.
     res (and the stored read result) are set to the value allRight,
     endOfLine, or endOfInput.
  *)
VAR
   did: IOLink.DeviceId ;
   dtp: IOLink.DeviceTablePtr ;
BEGIN
   CheckValid(cid) ;
   did := RTio.GetDeviceId(cid) ;
   dtp := IOLink.DeviceTablePtrValue(cid, did) ;
   IF dtp=NIL
   THEN
      RAISE(iochan, ORD(hardDeviceError),
            'IOChan.SkipLook: device table ptr is NIL')
   ELSE
      WITH dtp^ DO
         IF (cid=StdChans.NullChan()) OR (result=IOConsts.endOfInput)
         THEN
            RAISE(iochan, ORD(skipAtEnd),
                  'IOChan.SkipLook: attempt to skip data from a stream that has ended')
         ELSIF (ChanConsts.readFlag IN flags) AND (ChanConsts.textFlag IN flags)
         THEN
            doSkipLook(dtp, ch, result)
         ELSE
            RAISE(iochan, ORD(notAvailable),
                  'IOChan.SkipLook: attempt to skip data from a channel which is not configured as read and text')
         END ;
         res := result
      END
   END
END SkipLook ;


PROCEDURE WriteLn (cid: ChanId) ;
  (* Writes a line mark over the channel cid. *)
VAR
   did: IOLink.DeviceId ;
   dtp: IOLink.DeviceTablePtr ;
BEGIN
   CheckValid(cid) ;
   did := RTio.GetDeviceId(cid) ;
   dtp := IOLink.DeviceTablePtrValue(cid, did) ;
   IF dtp=NIL
   THEN
      RAISE(iochan, ORD(hardDeviceError),
            'IOChan.WriteLn: device table ptr is NIL')
   ELSE
      WITH dtp^ DO
         IF cid=StdChans.NullChan()
         THEN
            (* do nothing *)
         ELSIF (ChanConsts.writeFlag IN flags) AND (ChanConsts.textFlag IN flags)
         THEN
            dtp^.doLnWrite(dtp)
         ELSE
            RAISE(iochan, ORD(notAvailable),
                  'IOChan.WriteLn: attempting to write to a channel which is not configured as write and text')
         END
      END
   END
END WriteLn ;


PROCEDURE TextRead (cid: ChanId;
                    to: SYSTEM.ADDRESS;
                    maxChars: CARDINAL;
                    VAR charsRead: CARDINAL) ;
  (* Reads at most maxChars characters from the current line in cid,
     and assigns corresponding values to successive components of an
     ARRAY OF CHAR variable for which the address of the first
     component is to. The number of characters read is assigned
     to charsRead. The stored read result is set to allRight,
     endOfLine, or endOfInput.
  *)
VAR
   did: IOLink.DeviceId ;
   dtp: IOLink.DeviceTablePtr ;
BEGIN
   CheckValid(cid) ;
   did := RTio.GetDeviceId(cid) ;
   dtp := IOLink.DeviceTablePtrValue(cid, did) ;
   IF dtp=NIL
   THEN
      RAISE(iochan, ORD(hardDeviceError),
            'IOChan.TextRead: device table ptr is NIL')
   ELSE
      WITH dtp^ DO
         IF (cid=StdChans.NullChan()) OR (result=IOConsts.endOfInput)
         THEN
            charsRead := 0 ;
            result := IOConsts.endOfInput
         ELSIF (ChanConsts.readFlag IN flags) AND (ChanConsts.textFlag IN flags)
         THEN
            doTextRead(dtp, to, maxChars, charsRead)
         ELSE
            RAISE(iochan, ORD(notAvailable),
                  'IOChan.TextRead: attempt to read data from a channel which is not configured as read and text')
         END
      END
   END
END TextRead ;


PROCEDURE TextWrite (cid: ChanId;
                     from: SYSTEM.ADDRESS;
                     charsToWrite: CARDINAL) ;
  (* Writes a number of characters given by the value of charsToWrite,
     from successive components of an ARRAY OF CHAR variable for which
     the address of the first component is from, to the channel cid.
  *)
VAR
   did: IOLink.DeviceId ;
   dtp: IOLink.DeviceTablePtr ;
BEGIN
   CheckValid(cid) ;
   did := RTio.GetDeviceId(cid) ;
   dtp := IOLink.DeviceTablePtrValue(cid, did) ;
   IF dtp=NIL
   THEN
      RAISE(iochan, ORD(hardDeviceError),
            'IOChan.TextWrite: device table ptr is NIL')
   ELSE
      WITH dtp^ DO
         IF cid=StdChans.NullChan()
         THEN
            (* do nothing *)
         ELSIF (ChanConsts.writeFlag IN flags) AND (ChanConsts.textFlag IN flags)
         THEN
            doTextWrite(dtp, from, charsToWrite)
         ELSE
            RAISE(iochan, ORD(notAvailable),
                  'IOChan.TextWrite: attempt to write data to a channel which is not configured as write and text')
         END
      END
   END
END TextWrite ;


  (* Direct raw operations - these do not effect translation between
     the internal and external representation of data
  *)

PROCEDURE RawRead (cid: ChanId;
                   to: SYSTEM.ADDRESS;
                   maxLocs: CARDINAL;
                   VAR locsRead: CARDINAL) ;
  (* Reads at most maxLocs items from cid, and assigns corresponding
     values to successive components of an ARRAY OF LOC variable for
     which the address of the first component is to. The number of
     characters read is assigned to locsRead. The stored read result
     is set to the value allRight, or endOfInput.
  *)
VAR
   did: IOLink.DeviceId ;
   dtp: IOLink.DeviceTablePtr ;
BEGIN
   CheckValid(cid) ;
   did := RTio.GetDeviceId(cid) ;
   dtp := IOLink.DeviceTablePtrValue(cid, did) ;
   IF dtp=NIL
   THEN
      RAISE(iochan, ORD(hardDeviceError),
            'IOChan.RawRead: device table ptr is NIL')
   ELSE
      WITH dtp^ DO
         IF (cid=StdChans.NullChan()) OR (result=IOConsts.endOfInput)
         THEN
            locsRead := 0 ;
            result := IOConsts.endOfInput
         ELSIF (ChanConsts.readFlag IN flags) AND (ChanConsts.rawFlag IN flags)
         THEN
            doRawRead(dtp, to, maxLocs, locsRead)
         ELSE
            RAISE(iochan, ORD(notAvailable),
                  'IOChan.RawRead: attempt to read data from a channel which is not configured as read and raw')
         END
      END
   END
END RawRead ;


PROCEDURE RawWrite (cid: ChanId; from: SYSTEM.ADDRESS; locsToWrite: CARDINAL) ;
  (* Writes a number of items given by the value of charsToWrite,
     from successive components of an ARRAY OF LOC variable for
     which the address of the first component is from, to the channel cid.
  *)
VAR
   did: IOLink.DeviceId ;
   dtp: IOLink.DeviceTablePtr ;
BEGIN
   CheckValid(cid) ;
   did := RTio.GetDeviceId(cid) ;
   dtp := IOLink.DeviceTablePtrValue(cid, did) ;
   IF dtp=NIL
   THEN
      RAISE(iochan, ORD(hardDeviceError),
            'IOChan.RawWrite: device table ptr is NIL')
   ELSE
      WITH dtp^ DO
         IF (cid=StdChans.NullChan()) OR (result=IOConsts.endOfInput)
         THEN
            result := IOConsts.endOfInput
         ELSIF (ChanConsts.writeFlag IN flags) AND (ChanConsts.rawFlag IN flags)
         THEN
            doRawWrite(dtp, from, locsToWrite)
         ELSE
            RAISE(iochan, ORD(notAvailable),
                  'IOChan.RawWrite: attempt to write data to a channel which is not configured as write and raw')
         END
      END
   END
END RawWrite ;


  (* Common operations *)

PROCEDURE GetName (cid: ChanId; VAR s: ARRAY OF CHAR) ;
  (* Copies to s a name associated with the channel cid, possibly truncated
     (depending on the capacity of s).
  *)
VAR
   did: IOLink.DeviceId ;
   dtp: IOLink.DeviceTablePtr ;
BEGIN
   CheckValid(cid) ;
   did := RTio.GetDeviceId(cid) ;
   dtp := IOLink.DeviceTablePtrValue(cid, did) ;
   IF dtp=NIL
   THEN
      RAISE(iochan, ORD(hardDeviceError),
            'IOChan.GetName: device table ptr is NIL')
   ELSE
      dtp^.doGetName(dtp, s)
   END
END GetName ;


PROCEDURE Reset (cid: ChanId) ;
  (* Resets the channel cid to a state defined by the device module. *)
VAR
   did: IOLink.DeviceId ;
   dtp: IOLink.DeviceTablePtr ;
BEGIN
   CheckValid(cid) ;
   did := RTio.GetDeviceId(cid) ;
   dtp := IOLink.DeviceTablePtrValue(cid, did) ;
   IF dtp=NIL
   THEN
      RAISE(iochan, ORD(hardDeviceError),
            'IOChan.Reset: device table ptr is NIL')
   ELSE
      dtp^.doReset(dtp)
   END
END Reset ;


PROCEDURE Flush (cid: ChanId) ;
  (* Flushes any data buffered by the device module out to the channel cid. *)
VAR
   did: IOLink.DeviceId ;
   dtp: IOLink.DeviceTablePtr ;
BEGIN
   CheckValid(cid) ;
   did := RTio.GetDeviceId(cid) ;
   dtp := IOLink.DeviceTablePtrValue(cid, did) ;
   IF dtp=NIL
   THEN
      RAISE(iochan, ORD(hardDeviceError),
            'IOChan.Flush: device table ptr is NIL')
   ELSE
      dtp^.doFlush(dtp)
   END
END Flush ;


  (* Access to read results *)

PROCEDURE SetReadResult (cid: ChanId; res: IOConsts.ReadResults) ;
  (* Sets the read result value for the channel cid to the value res. *)
VAR
   did: IOLink.DeviceId ;
   dtp: IOLink.DeviceTablePtr ;
BEGIN
   CheckValid(cid) ;
   did := RTio.GetDeviceId(cid) ;
   dtp := IOLink.DeviceTablePtrValue(cid, did) ;
   IF dtp=NIL
   THEN
      RAISE(iochan, ORD(hardDeviceError),
            'IOChan.SetReadResult: device table ptr is NIL')
   ELSE
      dtp^.result := res
   END
END SetReadResult ;


PROCEDURE ReadResult (cid: ChanId) : IOConsts.ReadResults ;
  (* Returns the stored read result value for the channel cid.
     (This is initially the value notKnown).
  *)
VAR
   did: IOLink.DeviceId ;
   dtp: IOLink.DeviceTablePtr ;
BEGIN
   CheckValid(cid) ;
   did := RTio.GetDeviceId(cid) ;
   dtp := IOLink.DeviceTablePtrValue(cid, did) ;
   IF dtp=NIL
   THEN
      RAISE(iochan, ORD(hardDeviceError),
            'IOChan.SetReadResult: device table ptr is NIL') ;
      RETURN IOConsts.notKnown
   ELSE
      RETURN( dtp^.result )
   END
END ReadResult ;


  (* Users can discover which flags actually apply to a channel *)

PROCEDURE CurrentFlags (cid: ChanId) : ChanConsts.FlagSet ;
  (* Returns the set of flags that currently apply to the channel cid. *)
VAR
   did  : IOLink.DeviceId ;
   dtp  : IOLink.DeviceTablePtr ;
   empty: ChanConsts.FlagSet ;
BEGIN
   CheckValid(cid) ;
   did := RTio.GetDeviceId(cid) ;
   dtp := IOLink.DeviceTablePtrValue(cid, did) ;
   IF dtp=NIL
   THEN
      RAISE(iochan, ORD(hardDeviceError),
            'IOChan.SetReadResult: device table ptr is NIL') ;
      empty := ChanConsts.FlagSet {} ;
      RETURN empty
   ELSE
      RETURN( dtp^.flags )
   END
END CurrentFlags ;


  (* The following exceptions are defined for this module and its clients *)

PROCEDURE IsChanException () : BOOLEAN ;
  (* Returns TRUE if the current coroutine is in the exceptional
     execution state because of the raising of an exception from
     ChanExceptions; otherwise returns FALSE.
  *)
BEGIN
   RETURN( IsExceptionalExecution() AND IsCurrentSource(iochan) )
END IsChanException ;


PROCEDURE ChanException () : ChanExceptions ;
  (* If the current coroutine is in the exceptional execution state
     because of the raising of an exception from ChanExceptions,
     returns the corresponding enumeration value, and otherwise
     raises an exception.
  *)
BEGIN
   IF IsChanException()
   THEN
      RETURN( VAL(ChanExceptions, EXCEPTIONS.CurrentNumber(iochan)) )
   ELSE
      M2RTS.NoException (SYSTEM.ADR(__FILE__), __LINE__,
                        __COLUMN__, SYSTEM.ADR(__FUNCTION__),
                        SYSTEM.ADR ("not in the exceptional execution state"))
   END
END ChanException ;


  (* When a device procedure detects a device error, it raises the
     exception softDeviceError or hardDeviceError.  If these exceptions
     are handled, the following facilities may be used to discover
     an implementation-defined error number for the channel.
  *)

PROCEDURE DeviceError (cid: ChanId) : DeviceErrNum ;
  (* If a device error exception has been raised for the channel cid,
     returns the error number stored by the device module.
  *)
VAR
   did: IOLink.DeviceId ;
   dtp: IOLink.DeviceTablePtr ;
BEGIN
   CheckValid(cid) ;
   did := RTio.GetDeviceId(cid) ;
   dtp := IOLink.DeviceTablePtrValue(cid, did) ;
   IF dtp=NIL
   THEN
      RAISE(iochan, ORD(hardDeviceError),
            'IOChan.DeviceError: device table ptr is NIL') ;
      RETURN DeviceError (invalid)
   ELSE
      RETURN( dtp^.errNum )
   END
END DeviceError ;


BEGIN
   AllocateSource(iochan) ;
   invalid := ChanId(RTio.InitChanId())
END IOChan.
