(* M2Emit.mod issue errors to the GCC error reporting substructure.

Copyright (C) 2019-2024 Free Software Foundation, Inc.
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

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  *)

IMPLEMENTATION MODULE M2Emit ;

IMPORT m2linemap ;

FROM M2LexBuf IMPORT TokenToLocation ;
FROM m2linemap IMPORT ErrorAtf, WarningAtf, NoteAtf, internal_error ;
FROM DynamicStrings IMPORT string ;
FROM SYSTEM IMPORT ADR ;


(*
   EmitError - pass the error to GCC.
*)

PROCEDURE EmitError (error, note: BOOLEAN; token: CARDINAL; message: String) ;
BEGIN
   IF error
   THEN
      ErrorAtf (TokenToLocation (token), string (message))
   ELSIF note
   THEN
      NoteAtf (TokenToLocation (token), string (message))
   ELSE
      WarningAtf (TokenToLocation (token), string (message))
   END
END EmitError ;


(*
   InternalError - issue an internal error, message.
*)

PROCEDURE InternalError (message: ARRAY OF CHAR) ;
BEGIN
   internal_error (ADR (message))
END InternalError ;


(*
   UnknownLocation - return the unknown location (using GCC linemap for cc1gm2)
                     and constants for gm2l and gm2m.
*)

PROCEDURE UnknownLocation () : location_t ;
BEGIN
   RETURN m2linemap.UnknownLocation ()
END UnknownLocation ;


(*
   BuiltinsLocation - return the builtins location (using GCC linemap for cc1gm2)
                      and constants for gm2l and gm2m.
*)

PROCEDURE BuiltinsLocation () : location_t ;
BEGIN
   RETURN m2linemap.BuiltinsLocation ()
END BuiltinsLocation ;


END M2Emit.
