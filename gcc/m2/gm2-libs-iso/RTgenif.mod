(* RTgenif.mod implement a generic device interface mechanism used by RTgen.

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

IMPLEMENTATION MODULE RTgenif ;

FROM Storage IMPORT ALLOCATE, DEALLOCATE ;

TYPE
   GenDevIF = POINTER TO RECORD
                            did       : DeviceId ;
                            dorc      : readchar ;
                            dourc     : unreadchar ;
                            dogeterrno: geterrno ;
                            dorbytes  : readbytes ;
                            dowbytes  : writebytes ;
                            dowrln    : writeln;
                            doeof     : iseof ;
                            doeoln    : iseoln ;
                            doerror   : iserror ;
                         END ;


(*
   InitGenDev - initializes a generic device.
*)

PROCEDURE InitGenDevIF (d     : DeviceId;
                        rc    : readchar;
                        urc   : unreadchar;
                        geterr: geterrno;
                        rbytes: readbytes;
                        wbytes: writebytes;
                        wl    : writeln;
                        eof   : iseof;
                        eoln  : iseoln;
                        iserr : iserror) : GenDevIF ;
VAR
   g: GenDevIF ;
BEGIN
   NEW(g) ;
   WITH g^ DO
      did := d ;
      dorc := rc ;
      dourc := urc ;
      dogeterrno := geterr ;
      dorbytes := rbytes ;
      dowbytes := wbytes ;
      dowrln := wl ;
      doeof := eof ;
      doeoln := eoln ;
      doerror := iserr
   END ;
   RETURN( g )
END InitGenDevIF ;


(*
   getDID - returns the device id belonging to this generic interface.
*)

PROCEDURE getDID (g: GenDevIF) : DeviceId ;
BEGIN
   RETURN( g^.did )
END getDID ;


(*
   doReadChar - returns the next character from the generic device.
*)

PROCEDURE doReadChar (g: GenDevIF; d: DeviceTablePtr) : CHAR ;
BEGIN
   RETURN( g^.dorc(g, d) )
END doReadChar ;


(*
   doUnReadChar - pushes back a character to the generic device.
*)

PROCEDURE doUnReadChar (g: GenDevIF; d: DeviceTablePtr; ch: CHAR) : CHAR ;
BEGIN
   RETURN( g^.dourc(g, d, ch) )
END doUnReadChar ;


(*
   doGetErrno - returns the errno relating to the generic device.
*)

PROCEDURE doGetErrno (g: GenDevIF; d: DeviceTablePtr) : INTEGER ;
BEGIN
   RETURN( g^.dogeterrno(g, d) )
END doGetErrno ;


(*
   doRBytes - attempts to read, n, bytes from the generic device.
              It set the actual amount read and returns a boolean
              to determine whether an error occurred.
*)

PROCEDURE doRBytes (g: GenDevIF; d: DeviceTablePtr;
                    to: ADDRESS; max: CARDINAL;
                    VAR actual: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( g^.dorbytes(g, d, to, max, actual) )
END doRBytes ;


(*
   doWBytes - attempts to write, n, bytes to the generic device.
              It sets the actual amount written and returns a
              boolean to determine whether an error occurred.
*)

PROCEDURE doWBytes (g: GenDevIF; d: DeviceTablePtr;
                    from: ADDRESS; max: CARDINAL;
                    VAR actual: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( g^.dowbytes(g, d, from, max, actual) )
END doWBytes ;


(*
   doWrLn - writes an end of line marker and returns
            TRUE if successful.
*)

PROCEDURE doWrLn (g: GenDevIF; d: DeviceTablePtr) : BOOLEAN ;
BEGIN
   RETURN( g^.dowrln(g, d) )
END doWrLn ;


(*
   isEOF - returns true if the end of file was reached.
*)

PROCEDURE isEOF (g: GenDevIF; d: DeviceTablePtr) : BOOLEAN ;
BEGIN
   RETURN( g^.doeof(g, d) )
END isEOF ;


(*
   isEOLN - returns true if the end of line was reached.
*)

PROCEDURE isEOLN (g: GenDevIF; d: DeviceTablePtr) : BOOLEAN ;
BEGIN
   RETURN( g^.doeoln(g, d) )
END isEOLN ;


(*
   isError - returns true if an error was seen in the device.
*)

PROCEDURE isError (g: GenDevIF; d: DeviceTablePtr) : BOOLEAN ;
BEGIN
   RETURN( g^.doerror(g, d) )
END isError ;   


(*
   KillGenDevIF - deallocates a generic device.
*)

PROCEDURE KillGenDevIF (g: GenDevIF) : GenDevIF ;
BEGIN
   DISPOSE(g) ;
   RETURN( NIL )
END KillGenDevIF ;


END RTgenif.
