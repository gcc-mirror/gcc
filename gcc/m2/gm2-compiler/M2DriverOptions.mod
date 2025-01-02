(* M2DriverOptions.mod provides procedures to handle driver options.

Copyright (C) 2011-2025 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE M2DriverOptions ;

FROM DynamicStrings IMPORT String, Length, InitString, Mark, Slice, EqualArray,
                           InitStringCharStar, ConCatChar, ConCat, KillString,
                           PushAllocation, PopAllocationExemption, char ;

FROM SArgs IMPORT GetArg, Narg ;
FROM M2Options IMPORT CppRemember ;


(*
   CppArgument - some options might have arguments, remember these as well.
*)

PROCEDURE CppArgument (i: CARDINAL; option: String) : CARDINAL ;
VAR
   arg: String ;
BEGIN
   IF GetArg (arg, i+1) AND (char (arg, 0) # '-')
   THEN
      (* arg exists and is not an option and might be an argument to a specific option.  *)
      IF EqualArray (option, '-I')
      THEN
         INC (i) ;
         CppRemember (arg)  (* arg will be a path for -I.  *)
      ELSIF EqualArray (option, '-D')
      THEN
         INC (i) ;
         CppRemember (arg)  (* arg will be define for -D.  *)
      ELSIF EqualArray (option, '-isystem')
      THEN
         INC (i) ;
         CppRemember (arg)  (* arg will be a path for -isystem.  *)
      ELSIF EqualArray (option, '-imultiarch')
      THEN
         INC (i) ;
         CppRemember (arg)  (* arg will be a definition for -imultiarch.  *)
      END
   END ;
   RETURN i
END CppArgument ;


(*
   ScanCppArgs - scans the cpp arguments and builds up the cpp command line.
*)

PROCEDURE ScanCppArgs (i: CARDINAL) : CARDINAL ;
VAR
   option: String ;
BEGIN
   IF GetArg (option, i) AND EqualArray (option, '-fcpp-begin')
   THEN
      INC (i) ;
      WHILE GetArg (option, i) DO
         IF EqualArray (option, '-fcpp-end')
         THEN
            RETURN i
         ELSE
            (* do not remember the filename.  *)
            IF char (option, 0)='-'
            THEN
               CppRemember (option) ;
               i := CppArgument (i, option)
            END
         END ;
         INC (i)
      END
   END ;
   RETURN i
END ScanCppArgs ;


END M2DriverOptions.
