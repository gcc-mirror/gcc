(* RTio.mod implements low level routines for creating and destroying ChanIds.

Copyright (C) 2008-2025 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE RTio ;

FROM Storage IMPORT ALLOCATE, DEALLOCATE ;


TYPE
   ChanId = POINTER TO RECORD
                          did  : IOLink.DeviceId ;
                          dtp  : IOLink.DeviceTablePtr ;
                          file : FIO.File ;
                       END ;


(*
   InitChanId - return a new ChanId.
*)

PROCEDURE InitChanId () : ChanId ;
VAR
   c: ChanId ;
BEGIN
   NEW(c) ;
   RETURN( c )
END InitChanId ;


(*
   InitChanId - deallocate a ChanId.
*)

PROCEDURE KillChanId (c: ChanId) : ChanId ;
BEGIN
   DISPOSE(c) ;
   RETURN( NIL )
END KillChanId ;


(*
   NilChanId - return a NIL pointer.
*)

PROCEDURE NilChanId () : ChanId ;
BEGIN
   RETURN( NIL )
END NilChanId ;


(*
   GetDeviceId - returns the device id, from, c.
*)

PROCEDURE GetDeviceId (c: ChanId) : IOLink.DeviceId ;
BEGIN
   RETURN( c^.did )
END GetDeviceId ;


(*
   SetDeviceId - returns the device id, from, c.
*)

PROCEDURE SetDeviceId (c: ChanId; d: IOLink.DeviceId) ;
BEGIN
   c^.did := d
END SetDeviceId ;


(*
   GetDevicePtr - returns the device table ptr, from, c.
*)

PROCEDURE GetDevicePtr (c: ChanId) : IOLink.DeviceTablePtr ;
BEGIN
   RETURN( c^.dtp )
END GetDevicePtr ;

(*
   SetDevicePtr - sets the device table ptr in, c.
*)

PROCEDURE SetDevicePtr (c: ChanId; p: IOLink.DeviceTablePtr) ;
BEGIN
   c^.dtp := p
END SetDevicePtr ;


(*
   GetFile - returns the file field from, c.
*)

PROCEDURE GetFile (c: ChanId) : FIO.File ;
BEGIN
   RETURN( c^.file )
END GetFile ;


(*
   SetFile - sets the file field in, c.
*)

PROCEDURE SetFile (c: ChanId; f: FIO.File) ;
BEGIN
   c^.file := f
END SetFile ;


END RTio.
