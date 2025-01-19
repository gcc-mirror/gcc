------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P R E P C O M P                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2002-2025, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package stores all preprocessing data for the compiler

with Namet; use Namet;
with Types; use Types;

package Prepcomp is

   procedure Add_Dependency (S : Source_File_Index);
   --  Add a dependency on a non-source file. This is used internally for the
   --  preprocessing data file and the preprocessing definition file, and also
   --  externally for non-temporary configuration pragmas files.

   procedure Add_Dependencies;
   --  Add dependencies on the preprocessing data file and the preprocessing
   --  definition files, if any, and the non-temporary configuration pragmas
   --  files, if any.

   procedure Check_Symbols;
   --  Check if there are preprocessing symbols on the command line and set
   --  preprocessing if there are some: all files are preprocessed with these
   --  symbols. This procedure should not be called if there is a preprocessing
   --  data file specified on the command line. Instead a call should be made
   --  to Parse_Preprocessing_Data_File.

   procedure Parse_Preprocessing_Data_File (N : File_Name_Type);
   --  Parse a preprocessing data file, specified with a -gnatep= switch

   procedure Prepare_To_Preprocess
     (Source               : File_Name_Type;
      Preprocessing_Needed : out Boolean);
   --  Prepare, if necessary, the preprocessor for a source file. If the source
   --  file needs to be preprocessed, Preprocessing_Needed is set to True.
   --  Otherwise, Preprocessing_Needed is set to False and no preprocessing
   --  needs to be done.

   procedure Process_Command_Line_Symbol_Definitions;
   --  Check symbol definitions that have been added by calls to procedure
   --  Add_Symbol_Definition and stored as pointers to string, and put them in
   --  a table. The reason the definitions were stored as pointer to strings is
   --  that the name table is not yest initialized when we process the command
   --  line switches. These symbol definitions will be later used in
   --  the call to Prepare_To_Preprocess.

end Prepcomp;
