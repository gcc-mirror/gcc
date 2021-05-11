------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                    ADA.STRINGS.TEXT_OUTPUT.BASIC_FILES                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2020-2021, Free Software Foundation, Inc.       --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

private with GNAT.OS_Lib;
package Ada.Strings.Text_Output.Basic_Files is
   --  Normally, you should use Ada.Strings.Text_Output.Files, which
   --  automatically Closes files via finalization. If you don't want to use
   --  finalization, use this package instead. You must then Close the file by
   --  hand. The semantics is otherwise the same as Files.

   function Standard_Output return Sink_Access;
   function Standard_Error return Sink_Access;

   type File (<>) is new Sink with private;

   function Create_File
     (Name : String;
      Indent_Amount : Natural := Default_Indent_Amount;
      Chunk_Length : Positive := Default_Chunk_Length) return File;
   function Create_New_File
     (Name : String;
      Indent_Amount : Natural := Default_Indent_Amount;
      Chunk_Length : Positive := Default_Chunk_Length) return File;

   procedure Close (S : in out File'Class);

private

   package OS renames GNAT.OS_Lib;

   type File is new Sink with record
      FD : OS.File_Descriptor := OS.Invalid_FD;
   end record;

   overriding procedure Full_Method (S : in out File);
   overriding procedure Flush_Method (S : in out File);

end Ada.Strings.Text_Output.Basic_Files;
