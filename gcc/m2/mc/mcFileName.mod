(* Copyright (C) 2015-2024 Free Software Foundation, Inc.  *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  *)

IMPLEMENTATION MODULE mcFileName ;


FROM ASCII IMPORT nul ;
FROM DynamicStrings IMPORT InitString, Mark, Slice, Dup, ConCatChar, ConCat, Length, Equal, Index ;


CONST
   MaxFileName = 0 ;   (* zero means no limits *)
   MaxStemName = 0 ;
   Directory   = '/' ;


(*
   currently there are no limits on filename length, this may
   be incorrect on some systems.
*)


(*
   calculateFileName - calculates and returns a new string filename given a module
                       and an extension. String, Extension, is concatenated onto
                       Module and thus it is safe to `Mark' the extension for garbage
                       collection.
*)

PROCEDURE calculateFileName (module, extension: String) : String ;
BEGIN
   IF MaxFileName=0
   THEN
      RETURN ConCat (ConCatChar (Slice (module, 0, MaxFileName), '.'), extension)
   ELSE
      RETURN ConCat (ConCatChar (Slice (module, 0, MaxFileName-Length (extension)-1), '.'), extension)
   END
END calculateFileName ;


(*
   calculateStemName - calculates the stem name for given a module.
                       This name length will be operating system and
      	       	       compiler specific.
*)

PROCEDURE calculateStemName (module: String) : String ;
BEGIN
   RETURN Slice (module, 0, MaxStemName)
END calculateStemName ;


(*
   extractExtension - given a, filename, return the filename without
                      the extension, Ext.
*)

PROCEDURE extractExtension (filename, ext: String) : String ;
BEGIN
   IF Equal (ext, Mark (Slice (filename, -Length (ext), 0)))
   THEN
      RETURN Slice (filename, 0, -Length (ext))
   ELSE
      RETURN filename
   END
END extractExtension ;


(*
   extractModule - given a, filename, return the module name including any
                   extension. A new string is returned.
*)

PROCEDURE extractModule (filename: String) : String ;
VAR
   i: INTEGER ;
BEGIN
   i := Index (filename, Directory, 0) ;
   IF i=-1
   THEN
      RETURN Dup (filename)
   ELSE
      RETURN Slice (filename, i+1, 0)
   END
END extractModule ;


END mcFileName.
