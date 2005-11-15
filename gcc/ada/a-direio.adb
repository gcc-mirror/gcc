------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                        A D A . D I R E C T _ I O                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2005, Free Software Foundation, Inc.         --
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

--  This is the generic template for Direct_IO, i.e. the code that gets
--  duplicated. We absolutely minimize this code by either calling routines
--  in System.File_IO (for common file functions), or in System.Direct_IO
--  (for specialized Direct_IO functions)

with Interfaces.C_Streams; use Interfaces.C_Streams;
with System;               use System;
with System.CRTL;
with System.File_Control_Block;
with System.File_IO;
with System.Direct_IO;
with System.Storage_Elements;
with Unchecked_Conversion;

use type System.Direct_IO.Count;

package body Ada.Direct_IO is

   Zeroes : constant System.Storage_Elements.Storage_Array :=
              (1 .. System.Storage_Elements.Storage_Offset (Bytes) => 0);
   --  Buffer used to fill out partial records

   package FCB renames System.File_Control_Block;
   package FIO renames System.File_IO;
   package DIO renames System.Direct_IO;

   SU : constant := System.Storage_Unit;

   subtype AP      is FCB.AFCB_Ptr;
   subtype FP      is DIO.File_Type;
   subtype DPCount is DIO.Positive_Count;

   function To_FCB is new Unchecked_Conversion (File_Mode, FCB.File_Mode);
   function To_DIO is new Unchecked_Conversion (FCB.File_Mode, File_Mode);

   use type System.CRTL.size_t;

   -----------
   -- Close --
   -----------

   procedure Close (File : in out File_Type) is
   begin
      FIO.Close (AP (File));
   end Close;

   ------------
   -- Create --
   ------------

   procedure Create
     (File : in out File_Type;
      Mode : in File_Mode := Inout_File;
      Name : in String := "";
      Form : in String := "")
   is
   begin
      DIO.Create (FP (File), To_FCB (Mode), Name, Form);
      File.Bytes := Bytes;
   end Create;

   ------------
   -- Delete --
   ------------

   procedure Delete (File : in out File_Type) is
   begin
      FIO.Delete (AP (File));
   end Delete;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File (File : in File_Type) return Boolean is
   begin
      return DIO.End_Of_File (FP (File));
   end End_Of_File;

   ----------
   -- Form --
   ----------

   function Form (File : in File_Type) return String is
   begin
      return FIO.Form (AP (File));
   end Form;

   -----------
   -- Index --
   -----------

   function Index (File : in File_Type) return Positive_Count is
   begin
      return Positive_Count (DIO.Index (FP (File)));
   end Index;

   -------------
   -- Is_Open --
   -------------

   function Is_Open (File : in File_Type) return Boolean is
   begin
      return FIO.Is_Open (AP (File));
   end Is_Open;

   ----------
   -- Mode --
   ----------

   function Mode (File : in File_Type) return File_Mode is
   begin
      return To_DIO (FIO.Mode (AP (File)));
   end Mode;

   ----------
   -- Name --
   ----------

   function Name (File : in File_Type) return String is
   begin
      return FIO.Name (AP (File));
   end Name;

   ----------
   -- Open --
   ----------

   procedure Open
     (File : in out File_Type;
      Mode : in File_Mode;
      Name : in String;
      Form : in String := "")
   is
   begin
      DIO.Open (FP (File), To_FCB (Mode), Name, Form);
      File.Bytes := Bytes;
   end Open;

   ----------
   -- Read --
   ----------

   procedure Read
     (File : in File_Type;
      Item : out Element_Type;
      From : in Positive_Count)
   is
   begin
      --  For a non-constrained variant record type, we read into an
      --  intermediate buffer, since we may have the case of discriminated
      --  records where a discriminant check is required, and we may need
      --  to assign only part of the record buffer originally written.

      --  Note: we have to turn warnings on/off because this use of
      --  the Constrained attribute is an obsolescent feature.

      pragma Warnings (Off);
      if not Element_Type'Constrained then
         pragma Warnings (On);

         declare
            Buf : Element_Type;

         begin
            DIO.Read (FP (File), Buf'Address, Bytes, DPCount (From));
            Item := Buf;
         end;

      --  In the normal case, we can read straight into the buffer

      else
         DIO.Read (FP (File), Item'Address, Bytes, DPCount (From));
      end if;
   end Read;

   procedure Read (File : in File_Type; Item : out Element_Type) is
   begin
      --  Same processing for unconstrained case as above

      --  Note: we have to turn warnings on/off because this use of
      --  the Constrained attribute is an obsolescent feature.

      pragma Warnings (Off);
      if not Element_Type'Constrained then
         pragma Warnings (On);

         declare
            Buf : Element_Type;

         begin
            DIO.Read (FP (File), Buf'Address, Bytes);
            Item := Buf;
         end;

      else
         DIO.Read (FP (File), Item'Address, Bytes);
      end if;
   end Read;

   -----------
   -- Reset --
   -----------

   procedure Reset (File : in out File_Type; Mode : in File_Mode) is
   begin
      DIO.Reset (FP (File), To_FCB (Mode));
   end Reset;

   procedure Reset (File : in out File_Type) is
   begin
      DIO.Reset (FP (File));
   end Reset;

   ---------------
   -- Set_Index --
   ---------------

   procedure Set_Index (File : in File_Type; To : in Positive_Count) is
   begin
      DIO.Set_Index (FP (File), DPCount (To));
   end Set_Index;

   ----------
   -- Size --
   ----------

   function Size (File : in File_Type) return Count is
   begin
      return Count (DIO.Size (FP (File)));
   end Size;

   -----------
   -- Write --
   -----------

   procedure Write
     (File : in File_Type;
      Item : in Element_Type;
      To   : in Positive_Count)
   is
   begin
      DIO.Set_Index (FP (File), DPCount (To));
      DIO.Write (FP (File), Item'Address, Item'Size / SU, Zeroes);
   end Write;

   procedure Write (File : in File_Type; Item : in Element_Type) is
   begin
      DIO.Write (FP (File), Item'Address, Item'Size / SU, Zeroes);
   end Write;

end Ada.Direct_IO;
