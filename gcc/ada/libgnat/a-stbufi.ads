------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                      ADA.STRINGS.TEXT_BUFFERS.FILES                      --
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

with Ada.Finalization;
with System.OS_Lib;

package Ada.Strings.Text_Buffers.Files is

   type File_Buffer is new Root_Buffer_Type with private;
   --  Output written to a File_Buffer is written to the associated file.

   function Create_From_FD
     (FD                      : System.OS_Lib.File_Descriptor;
      Close_Upon_Finalization : Boolean := True)
     return File_Buffer;
   --  file closed upon finalization if specified

   function Create_File (Name : String) return File_Buffer;
   --  file closed upon finalization

   function Create_Standard_Output_Buffer return File_Buffer is
     (Create_From_FD (System.OS_Lib.Standout,
                      Close_Upon_Finalization => False));
   function Create_Standard_Error_Buffer return File_Buffer is
     (Create_From_FD (System.OS_Lib.Standerr,
                      Close_Upon_Finalization => False));

private

   procedure Put_UTF_8_Implementation
     (Buffer : in out Root_Buffer_Type'Class;
      Item : UTF_Encoding.UTF_8_String)
     with Pre => Buffer in File_Buffer'Class;

   package Mapping is new Output_Mapping (Put_UTF_8_Implementation);

   package OS renames System.OS_Lib;

   type Self_Ref (Self : not null access File_Buffer)
     is new Finalization.Limited_Controlled with null record;
   overriding procedure Finalize (Ref : in out Self_Ref);

   type File_Buffer is new Mapping.Buffer_Type with record
      FD  : OS.File_Descriptor := OS.Invalid_FD;
      Ref : Self_Ref (File_Buffer'Access);
      Close_Upon_Finalization : Boolean := False;
   end record;

end Ada.Strings.Text_Buffers.Files;
