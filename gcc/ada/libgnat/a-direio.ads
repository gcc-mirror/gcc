------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                        A D A . D I R E C T _ I O                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2017, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

with Ada.IO_Exceptions;
with System.Direct_IO;
with Interfaces.C_Streams;

generic
   type Element_Type is private;

package Ada.Direct_IO is

   pragma Compile_Time_Warning
     (Element_Type'Has_Access_Values,
      "Element_Type for Direct_IO instance has access values");

   pragma Compile_Time_Warning
     (Element_Type'Has_Tagged_Values,
      "Element_Type for Direct_IO instance has tagged values");

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
      Mode : File_Mode := Inout_File;
      Name : String := "";
      Form : String := "");

   procedure Open
     (File : in out File_Type;
      Mode : File_Mode;
      Name : String;
      Form : String := "");

   procedure Close  (File : in out File_Type);
   procedure Delete (File : in out File_Type);
   procedure Reset  (File : in out File_Type; Mode : File_Mode);
   procedure Reset  (File : in out File_Type);

   function Mode (File : File_Type) return File_Mode;
   function Name (File : File_Type) return String;
   function Form (File : File_Type) return String;

   function Is_Open (File : File_Type) return Boolean;

   procedure Flush (File : File_Type);

   ---------------------------------
   -- Input and Output Operations --
   ---------------------------------

   procedure Read
     (File : File_Type;
      Item : out Element_Type;
      From : Positive_Count);

   procedure Read
     (File : File_Type;
      Item : out Element_Type);

   procedure Write
     (File : File_Type;
      Item : Element_Type;
      To   : Positive_Count);

   procedure Write
     (File : File_Type;
      Item : Element_Type);

   procedure Set_Index (File : File_Type; To : Positive_Count);

   function Index (File : File_Type) return Positive_Count;
   function Size  (File : File_Type) return Count;

   function End_Of_File (File : File_Type) return Boolean;

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

   --  The following procedures have a File_Type formal of mode IN OUT because
   --  they may close the original file. The Close operation may raise an
   --  exception, but in that case we want any assignment to the formal to
   --  be effective anyway, so it must be passed by reference (or the caller
   --  will be left with a dangling pointer).

   pragma Export_Procedure
     (Internal  => Close,
      External  => "",
      Mechanism => Reference);
   pragma Export_Procedure
     (Internal  => Delete,
      External  => "",
      Mechanism => Reference);
   pragma Export_Procedure
     (Internal        => Reset,
      External        => "",
      Parameter_Types => (File_Type),
      Mechanism       => Reference);
   pragma Export_Procedure
     (Internal        => Reset,
      External        => "",
      Parameter_Types => (File_Type, File_Mode),
      Mechanism       => (File => Reference));

   type File_Type is new System.Direct_IO.File_Type;

   Bytes : constant Interfaces.C_Streams.size_t :=
             Interfaces.C_Streams.size_t'Max
               (1, Element_Type'Max_Size_In_Storage_Elements);
   --  Size of an element in storage units. The Max operation here is to ensure
   --  that we allocate a single byte for zero-sized elements. It's a bit weird
   --  to instantiate Direct_IO with zero sized elements, but it is legal and
   --  this adjustment ensures that we don't get anomalous behavior.

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
