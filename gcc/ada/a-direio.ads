------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                        A D A . D I R E C T _ I O                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2005 Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.IO_Exceptions;
with System.Direct_IO;
with Interfaces.C_Streams;

generic
   type Element_Type is private;

package Ada.Direct_IO is

   pragma Compile_Time_Warning
     (Element_Type'Has_Access_Values,
      "?Element_Type for Direct_'I'O instance has access values");

   type File_Type is limited private;

   type File_Mode is (In_File, Inout_File, Out_File);

   --  The following representation clause allows the use of unchecked
   --  conversion for rapid translation between the File_Mode type
   --  used in this package and System.File_IO.

   for File_Mode use
     (In_File    => 0,   -- System.File_IO.File_Mode'Pos (In_File)
      Inout_File => 1,   -- System.File_IO.File_Mode'Pos (Inout_File);
      Out_File   => 2);  -- System.File_IO.File_Mode'Pos (Out_File)

   type Count is range 0 .. System.Direct_IO.Count'Last;

   subtype Positive_Count is Count range 1 .. Count'Last;

   ---------------------
   -- File Management --
   ---------------------

   procedure Create
     (File : in out File_Type;
      Mode : in File_Mode := Inout_File;
      Name : in String := "";
      Form : in String := "");

   procedure Open
     (File : in out File_Type;
      Mode : in File_Mode;
      Name : in String;
      Form : in String := "");

   procedure Close  (File : in out File_Type);
   procedure Delete (File : in out File_Type);
   procedure Reset  (File : in out File_Type; Mode : in File_Mode);
   procedure Reset  (File : in out File_Type);

   function Mode (File : in File_Type) return File_Mode;
   function Name (File : in File_Type) return String;
   function Form (File : in File_Type) return String;

   function Is_Open (File : in File_Type) return Boolean;

   ---------------------------------
   -- Input and Output Operations --
   ---------------------------------

   procedure Read
     (File : in File_Type;
      Item : out Element_Type;
      From : in Positive_Count);

   procedure Read
     (File : in File_Type;
      Item : out Element_Type);

   procedure Write
     (File : in File_Type;
      Item : in Element_Type;
      To   : in Positive_Count);

   procedure Write
     (File : in File_Type;
      Item : in Element_Type);

   procedure Set_Index (File : in File_Type; To : in Positive_Count);

   function Index (File : in File_Type) return Positive_Count;
   function Size  (File : in File_Type) return Count;

   function End_Of_File (File : in File_Type) return Boolean;

   ----------------
   -- Exceptions --
   ----------------

   Status_Error : exception renames IO_Exceptions.Status_Error;
   Mode_Error   : exception renames IO_Exceptions.Mode_Error;
   Name_Error   : exception renames IO_Exceptions.Name_Error;
   Use_Error    : exception renames IO_Exceptions.Use_Error;
   Device_Error : exception renames IO_Exceptions.Device_Error;
   End_Error    : exception renames IO_Exceptions.End_Error;
   Data_Error   : exception renames IO_Exceptions.Data_Error;

private
   type File_Type is new System.Direct_IO.File_Type;

   Bytes : constant Interfaces.C_Streams.size_t :=
             Element_Type'Max_Size_In_Storage_Elements;
   --  Size of an element in storage units

   pragma Inline (Close);
   pragma Inline (Create);
   pragma Inline (Delete);
   pragma Inline (End_Of_File);
   pragma Inline (Form);
   pragma Inline (Index);
   pragma Inline (Is_Open);
   pragma Inline (Mode);
   pragma Inline (Name);
   pragma Inline (Open);
   pragma Inline (Read);
   pragma Inline (Reset);
   pragma Inline (Set_Index);
   pragma Inline (Size);
   pragma Inline (Write);

end Ada.Direct_IO;
