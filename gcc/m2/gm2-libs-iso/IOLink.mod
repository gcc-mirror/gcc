(* IOLink.mod implement the ISO IOLink specification.

Copyright (C) 2008-2021 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE IOLink ;

IMPORT RTio, RTentity, EXCEPTIONS, IOChan, M2RTS, SYSTEM, ASCII ;

FROM Storage IMPORT ALLOCATE, DEALLOCATE ;


(*
   Values of this type are used to identify new device modules,
   and are normally obtained by them during their initialization.
*)

TYPE
   DeviceId = POINTER TO RECORD
                            cids:  RTentity.Group ;
                         END ;
   resourceState = (allocated, deallocated) ;


VAR
   dids  :  RTentity.Group ;
   iolink:  EXCEPTIONS.ExceptionSource ;


(*
   checkValidDevice - checks to see that the, did, is
                      known to exist.
*)

PROCEDURE checkValidDevice (did: DeviceId) ;
BEGIN
   IF NOT RTentity.IsIn(dids, did)
   THEN
      EXCEPTIONS.RAISE(iolink, ORD(IOChan.wrongDevice),
                       'IOLink: device id specified does not exist')
   END
END checkValidDevice ;


(*
   Allocates a unique value of type DeviceId, and assigns this
   value to did.
*)

PROCEDURE AllocateDeviceId (VAR did: DeviceId) ;
BEGIN
   NEW(did) ;
   IF did=NIL
   THEN
      EXCEPTIONS.RAISE(iolink, ORD(IOChan.hardDeviceError),
                       'IOLink.AllocateDeviceId: out of memory error')
   ELSE
      RTentity.PutKey(dids, did, ORD(allocated)) ;
      WITH did^ DO
         cids := RTentity.InitGroup()
      END
   END
END AllocateDeviceId ;


PROCEDURE defaultLook (d: DeviceTablePtr;
                       VAR ch: CHAR;
                       VAR r : IOConsts.ReadResults) ;
BEGIN
   EXCEPTIONS.RAISE(iolink, ORD(IOChan.notAvailable),
                    'IOLink:Look operation on device is not available')
END defaultLook ;


PROCEDURE defaultSkip (d: DeviceTablePtr) ;
BEGIN
   EXCEPTIONS.RAISE(iolink, ORD(IOChan.notAvailable),
                    'IOLink:Skip operation on device is not available')
END defaultSkip ;


PROCEDURE defaultSkipLook (d: DeviceTablePtr;
                           VAR ch: CHAR;
                           VAR r : IOConsts.ReadResults) ;
BEGIN
   EXCEPTIONS.RAISE(iolink, ORD(IOChan.notAvailable),
                    'IOLink:SkipLook operation on device is not available')
END defaultSkipLook ;


PROCEDURE defaultWriteLn (d: DeviceTablePtr) ;
BEGIN
   EXCEPTIONS.RAISE(iolink, ORD(IOChan.notAvailable),
                    'IOLink:WriteLn operation on device is not available')
END defaultWriteLn ;


PROCEDURE defaultTextRead (d: DeviceTablePtr; a: SYSTEM.ADDRESS; n: CARDINAL; VAR r: CARDINAL) ;
BEGIN
   EXCEPTIONS.RAISE(iolink, ORD(IOChan.notAvailable),
                    'IOLink:TextRead operation on device is not available')
END defaultTextRead ;


PROCEDURE defaultTextWrite (d: DeviceTablePtr; a: SYSTEM.ADDRESS; n: CARDINAL) ;
BEGIN
   EXCEPTIONS.RAISE(iolink, ORD(IOChan.notAvailable),
                    'IOLink:TextWrite operation on device is not available')
END defaultTextWrite ;


PROCEDURE defaultRawRead (d: DeviceTablePtr; a: SYSTEM.ADDRESS; n: CARDINAL; VAR r: CARDINAL) ;
BEGIN
   EXCEPTIONS.RAISE(iolink, ORD(IOChan.notAvailable),
                    'IOLink:TextRawRead operation on device is not available')
END defaultRawRead ;


PROCEDURE defaultRawWrite (d: DeviceTablePtr; a: SYSTEM.ADDRESS; n: CARDINAL) ;
BEGIN
   EXCEPTIONS.RAISE(iolink, ORD(IOChan.notAvailable),
                    'IOLink:TextRawWrite operation on device is not available')
END defaultRawWrite ;


PROCEDURE defaultGetName (d: DeviceTablePtr; VAR a: ARRAY OF CHAR) ;
BEGIN
   a[0] := ASCII.nul
END defaultGetName ;


PROCEDURE defaultReset (d: DeviceTablePtr) ;
BEGIN
   (* do nothing *)
END defaultReset ;


PROCEDURE defaultFlush (d: DeviceTablePtr) ;
BEGIN
   (* do nothing *)
END defaultFlush ;


PROCEDURE defaultFree (d: DeviceTablePtr) ;
BEGIN
   (* do nothing *)
END defaultFree ;


(*
   InitDtp - creates a new DeviceTablePtr and initializes the
             fields to their defaults.
*)

PROCEDURE InitDtp (d: DeviceId; c: IOChan.ChanId) : DeviceTablePtr ;
VAR
   p: DeviceTablePtr ;
BEGIN
   NEW(p) ;
   WITH p^ DO
      cd  := NIL ;
      did := d ;
      cid := c ;
      result := IOConsts.notKnown ;
      errNum := 0 ;
      flags := ChanConsts.FlagSet{} ;
      doLook := defaultLook ;
      doSkip := defaultSkip ;
      doSkipLook := defaultSkipLook ;
      doLnWrite := defaultWriteLn ;
      doTextRead := defaultTextRead ;
      doTextWrite := defaultTextWrite ;
      doRawRead := defaultRawRead ;
      doRawWrite := defaultRawWrite ;
      doGetName := defaultGetName ;
      doReset := defaultReset ;
      doFlush := defaultFlush ;
      doFree := defaultFree ;
   END ;
   RETURN( p )
END InitDtp ;


(*
   KillDtp - deallocate, p, and any associated resource.
*)

PROCEDURE KillDtp (p: DeviceTablePtr) : DeviceTablePtr ;
BEGIN
   WITH p^ DO
      doFlush(p) ;
      doFree(p)
   END ;
   DISPOSE(p) ;
   RETURN( NIL )
END KillDtp ;


(*
   Attempts to make a new channel for the device module identified
   by did. If no more channels can be made, the identity of
   the invalid channel is assigned to cid.  Otherwise, the identity
   of a new channel is assigned to cid.
*)

PROCEDURE MakeChan (did: DeviceId; VAR cid: IOChan.ChanId) ;
BEGIN
   checkValidDevice(did) ;
   cid := IOChan.ChanId(RTio.InitChanId()) ;
   IF cid=NIL
   THEN
      cid := IOChan.InvalidChan()
   ELSE
      WITH did^ DO
         RTentity.PutKey(cids, cid, ORD(allocated))
      END ;
      RTio.SetDeviceId(cid, did) ;
      RTio.SetDevicePtr(cid, InitDtp(did, cid))
   END
END MakeChan ;


(*
   If the device module identified by did is not the module that
   made the channel identified by cid, the exception wrongDevice is
   raised; otherwise the channel is deallocated, and the value
   identifying the invalid channel is assigned to cid.
*)

PROCEDURE UnMakeChan (did: DeviceId; VAR cid: IOChan.ChanId) ;
BEGIN
   checkValidDevice(did) ;
   WITH did^ DO
      IF RTentity.IsIn(cids, cid)
      THEN
         RTio.SetDevicePtr(cid, KillDtp(RTio.GetDevicePtr(cid))) ;
         RTentity.DelKey(cids, cid) ;
         cid := IOChan.ChanId(RTio.KillChanId(cid)) ;
         cid := IOChan.InvalidChan()
      ELSE
         EXCEPTIONS.RAISE(iolink, ORD(IOChan.wrongDevice),
                         'IOLink.UnMakeChan: channel does not belong to device')
      END
   END
END UnMakeChan ;


(*
   The pointer to the device table for a channel is obtained using the
   following procedure:

   If the device module identified by did is not the module that made
   the channel identified by cid, the exception wrongDevice is raised.
*)

PROCEDURE DeviceTablePtrValue (cid: IOChan.ChanId;
                               did: DeviceId) : DeviceTablePtr ;
BEGIN
   checkValidDevice(did) ;
   WITH did^ DO
      IF RTentity.IsIn(cids, cid)
      THEN
         RETURN( RTio.GetDevicePtr(cid) )
      ELSE
         EXCEPTIONS.RAISE(iolink, ORD(IOChan.wrongDevice),
                          'IOLink.DeviceTablePtrValue: channel does belong to device')
      END
   END
END DeviceTablePtrValue ;


PROCEDURE IsDevice (cid: IOChan.ChanId; did: DeviceId) : BOOLEAN ;
  (* Tests if the device module identified by did is the module
     that made the channel identified by cid.
  *)
BEGIN
   IF RTentity.IsIn(dids, did)
   THEN
      WITH did^ DO
         RETURN( RTentity.IsIn(cids, cid) )
      END
   END ;
   RETURN( FALSE )
END IsDevice ;


PROCEDURE RAISEdevException (cid: IOChan.ChanId; did: DeviceId;
                             x: DevExceptionRange; s: ARRAY OF CHAR) ;
(* If the device module identified by did is not the module
   that made the channel identified by cid, the exception
   wrongDevice is raised; otherwise the given exception
   is raised, and the string value in s is included in the
   exception message.
*)
BEGIN
   checkValidDevice(did) ;
   WITH did^ DO
      IF RTentity.IsIn(cids, cid)
      THEN
         EXCEPTIONS.RAISE(iolink, ORD(x), s)
      ELSE
         EXCEPTIONS.RAISE(iolink, ORD(IOChan.wrongDevice),
                          'IOLink.RAISEdevException: channel does not belong to device')
      END
   END
END RAISEdevException ;


PROCEDURE IsIOException () : BOOLEAN ;
  (* Returns TRUE if the current coroutine is in the exceptional
     execution state because of the raising af an exception from
     ChanExceptions; otherwise FALSE.
  *)
BEGIN
   RETURN( EXCEPTIONS.IsExceptionalExecution() AND
           EXCEPTIONS.IsCurrentSource(iolink) )
END IsIOException ;


PROCEDURE IOException () : IOChan.ChanExceptions ;
  (* If the current coroutine is in the exceptional execution state
     because of the raising of an exception from ChanExceptions,
     returns the corresponding enumeration value, and otherwise
     raises an exception.
  *)
BEGIN
   IF IsIOException()
   THEN
      RETURN( VAL(IOChan.ChanExceptions,
                  EXCEPTIONS.CurrentNumber(iolink)) )
   ELSE
      M2RTS.NoException(SYSTEM.ADR(__FILE__), __LINE__,
                        __COLUMN__, SYSTEM.ADR(__FUNCTION__),
                        SYSTEM.ADR ("not in the exceptional execution state"))
   END
END IOException ;


(*
   Init - initialise global variables.
*)

PROCEDURE Init ;
BEGIN
   EXCEPTIONS.AllocateSource(iolink) ;
   dids := RTentity.InitGroup()
END Init ;


BEGIN
   Init
END IOLink.
