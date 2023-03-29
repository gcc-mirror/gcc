(* RTdata.mod implements a mechanism whereby devices can attach data.

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

IMPLEMENTATION MODULE RTdata ;

FROM IOLink IMPORT DeviceTablePtr, RAISEdevException ;
FROM RTentity IMPORT Group, InitGroup, PutKey, IsIn, DelKey ;
FROM Storage IMPORT ALLOCATE, DEALLOCATE ;

IMPORT IOChan ;

TYPE
   key = (allocated, deallocated) ;

   (* each module can register one of these *)
   ModuleId = POINTER TO RECORD
                            id: CARDINAL ;
                         END ;

   (* each device can contain multiple of the these *)
   ModuleData = POINTER TO RECORD
                              mid  : ModuleId ;
                              data : ADDRESS ;
                              free : FreeProcedure ;
                              right: ModuleData ;
                           END ;


(*
   MakeModuleId - creates a unique module Id.
*)

PROCEDURE MakeModuleId (VAR m: ModuleId) ;
BEGIN
   INC(lastId) ;
   NEW(m) ;
   WITH m^ DO
      id := lastId
   END ;
   PutKey(mids, m, ORD(allocated))
END MakeModuleId ;


(*
   verifyModuleId - verifies that, m, has not been deallocated.
*)

PROCEDURE verifyModuleId (m: ModuleId; d: DeviceTablePtr) ;
BEGIN
   IF NOT IsIn(mids, m)
   THEN
      WITH d^ DO
         RAISEdevException(cid, did, IOChan.hardDeviceError,
                           'RTdata: invalid module id')
      END
   END
END verifyModuleId ;


(*
   InitData - adds, datum, to the device, d.  The datum
              is associated with ModuleID, m.
*)

PROCEDURE InitData (d: DeviceTablePtr; m: ModuleId;
                    datum: ADDRESS; f: FreeProcedure) ;
VAR
   md: ModuleData ;
BEGIN
   NEW(md) ;
   WITH md^ DO
      mid := m ;
      data := datum ;
      free := f ;
      right := d^.cd
   END ;
   d^.cd := md
END InitData ;


(*
   GetData - returns the datum assocated with ModuleId, m.
*)

PROCEDURE GetData (d: DeviceTablePtr; m: ModuleId) : ADDRESS ;
VAR
   md: ModuleData ;
BEGIN
   verifyModuleId(m, d) ;
   md := d^.cd ;
   WHILE md#NIL DO
      IF md^.mid=m
      THEN
         RETURN( md^.data )
      ELSE
         md := md^.right
      END
   END ;
   RETURN( md )
END GetData ;


(*
   KillData - destroys the datum associated with ModuleId, m,
              in device, d.  It invokes the free procedure
              given during InitData.
*)

PROCEDURE KillData (d: DeviceTablePtr; m: ModuleId) ;
VAR
   last,
   md  : ModuleData ;
BEGIN
   verifyModuleId(m, d) ;
   last := NIL ;
   md := d^.cd ;
   WHILE md#NIL DO
      IF md^.mid=m
      THEN
         IF last=NIL
         THEN
            d^.cd := md^.right
         ELSE
            last^.right := md^.right
         END ;
         DelKey(mids, m) ;
         md^.free(md^.data) ;
         md^.data := NIL ;
         DISPOSE(md)
      ELSE
         last := md ;
         md := md^.right
      END
   END
END KillData ;


VAR
   mids  : Group ;
   lastId: CARDINAL ;
BEGIN
   mids := InitGroup() ;
   lastId := 0
END RTdata.
