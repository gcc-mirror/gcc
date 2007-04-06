------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     S Y S T E M . D I R E C T _ I O                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2006, Free Software Foundation, Inc.         --
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

--  This package contains the declaration of the control block used for
--  Direct_IO. This must be declared at the outer library level. It also
--  contains code that is shared between instances of Direct_IO.

with Interfaces.C_Streams;
with Ada.Streams;
with System.File_Control_Block;
with System.Storage_Elements;

package System.Direct_IO is

   package FCB renames System.File_Control_Block;

   type Operation is (Op_Read, Op_Write, Op_Other);
   --  Type used to record last operation (to optimize sequential operations)

   subtype Count is Interfaces.C_Streams.long;
   --  The Count type in each instantiation is derived from this type

   subtype Positive_Count is Count range 1 .. Count'Last;

   type Direct_AFCB is new FCB.AFCB with record
      Index : Count := 1;
      --  Current Index value

      Bytes : Interfaces.C_Streams.size_t;
      --  Length of item in bytes (set from inside generic template)

      Last_Op : Operation := Op_Other;
      --  Last operation performed on file, used to avoid unnecessary
      --  repositioning between successive read or write operations.
   end record;

   function AFCB_Allocate (Control_Block : Direct_AFCB) return FCB.AFCB_Ptr;

   procedure AFCB_Close (File : not null access Direct_AFCB);
   procedure AFCB_Free  (File : not null access Direct_AFCB);

   procedure Read
     (File : in out Direct_AFCB;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);
   --  Required overriding of Read, not actually used for Direct_IO

   procedure Write
     (File : in out Direct_AFCB;
      Item : Ada.Streams.Stream_Element_Array);
   --  Required overriding of Write, not actually used for Direct_IO

   type File_Type is access all Direct_AFCB;
   --  File_Type in individual instantiations is derived from this type

   procedure Create
     (File : in out File_Type;
      Mode : FCB.File_Mode := FCB.Inout_File;
      Name : String := "";
      Form : String := "");

   function End_Of_File (File : File_Type) return Boolean;

   function Index (File : File_Type) return Positive_Count;

   procedure Open
     (File : in out File_Type;
      Mode : FCB.File_Mode;
      Name : String;
      Form : String := "");

   procedure Read
     (File : File_Type;
      Item : System.Address;
      Size : Interfaces.C_Streams.size_t;
      From : Positive_Count);

   procedure Read
     (File : File_Type;
      Item : System.Address;
      Size : Interfaces.C_Streams.size_t);

   procedure Reset (File : in out File_Type; Mode : FCB.File_Mode);

   procedure Reset (File : in out File_Type);

   procedure Set_Index (File : File_Type; To : Positive_Count);

   function Size (File : File_Type) return Count;

   procedure Write
     (File   : File_Type;
      Item   : System.Address;
      Size   : Interfaces.C_Streams.size_t;
      Zeroes : System.Storage_Elements.Storage_Array);
   --  Note: Zeroes is the buffer of zeroes used to fill out partial records

end System.Direct_IO;
