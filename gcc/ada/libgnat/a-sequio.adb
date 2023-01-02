------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                    A D A . S E Q U E N T I A L _ I O                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2023, Free Software Foundation, Inc.         --
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

--  This is the generic template for Sequential_IO, i.e. the code that gets
--  duplicated. We absolutely minimize this code by either calling routines
--  in System.File_IO (for common file functions), or in System.Sequential_IO
--  (for specialized Sequential_IO functions)

with Ada.Unchecked_Conversion;

with System;
with System.Byte_Swapping;
with System.CRTL;
with System.File_Control_Block;
with System.File_IO;
with System.Storage_Elements;

with Interfaces.C_Streams; use Interfaces.C_Streams;

package body Ada.Sequential_IO is

   package FIO renames System.File_IO;
   package FCB renames System.File_Control_Block;
   package SIO renames System.Sequential_IO;
   package SSE renames System.Storage_Elements;

   SU : constant := System.Storage_Unit;

   subtype AP is FCB.AFCB_Ptr;
   subtype FP is SIO.File_Type;

   function To_FCB is new Ada.Unchecked_Conversion (File_Mode, FCB.File_Mode);
   function To_SIO is new Ada.Unchecked_Conversion (FCB.File_Mode, File_Mode);

   use type System.Bit_Order;
   use type System.CRTL.size_t;

   procedure Byte_Swap (Siz : in out size_t);
   --  Byte swap Siz

   ---------------
   -- Byte_Swap --
   ---------------

   procedure Byte_Swap (Siz : in out size_t) is
      use System.Byte_Swapping;
   begin
      case size_t'Size is
         when 32     => Siz := size_t (Bswap_32 (U32 (Siz)));
         when 64     => Siz := size_t (Bswap_64 (U64 (Siz)));
         when others => raise Program_Error;
      end case;
   end Byte_Swap;

   -----------
   -- Close --
   -----------

   procedure Close (File : in out File_Type) is
   begin
      FIO.Close (AP (File)'Unrestricted_Access);
   end Close;

   ------------
   -- Create --
   ------------

   procedure Create
     (File : in out File_Type;
      Mode : File_Mode := Out_File;
      Name : String := "";
      Form : String := "")
   is
   begin
      SIO.Create (FP (File), To_FCB (Mode), Name, Form);
   end Create;

   ------------
   -- Delete --
   ------------

   procedure Delete (File : in out File_Type) is
   begin
      FIO.Delete (AP (File)'Unrestricted_Access);
   end Delete;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File (File : File_Type) return Boolean is
   begin
      return FIO.End_Of_File (AP (File));
   end End_Of_File;

   -----------
   -- Flush --
   -----------

   procedure Flush (File : File_Type) is
   begin
      FIO.Flush (AP (File));
   end Flush;

   ----------
   -- Form --
   ----------

   function Form (File : File_Type) return String is
   begin
      return FIO.Form (AP (File));
   end Form;

   -------------
   -- Is_Open --
   -------------

   function Is_Open (File : File_Type) return Boolean is
   begin
      return FIO.Is_Open (AP (File));
   end Is_Open;

   ----------
   -- Mode --
   ----------

   function Mode (File : File_Type) return File_Mode is
   begin
      return To_SIO (FIO.Mode (AP (File)));
   end Mode;

   ----------
   -- Name --
   ----------

   function Name (File : File_Type) return String is
   begin
      return FIO.Name (AP (File));
   end Name;

   ----------
   -- Open --
   ----------

   procedure Open
     (File : in out File_Type;
      Mode : File_Mode;
      Name : String;
      Form : String := "")
   is
   begin
      SIO.Open (FP (File), To_FCB (Mode), Name, Form);
   end Open;

   ----------
   -- Read --
   ----------

   procedure Read (File : File_Type; Item : out Element_Type) is
      Siz  : constant size_t := (Item'Size + SU - 1) / SU;
      Rsiz : size_t;

   begin
      FIO.Check_Read_Status (AP (File));

      --  For non-definite type or type with discriminants, read size and
      --  raise Program_Error if it is larger than the size of the item.

      if not Element_Type'Definite
        or else Element_Type'Has_Discriminants
      then
         FIO.Read_Buf
           (AP (File), Rsiz'Address, size_t'Size / System.Storage_Unit);

         --  If item read has non-default scalar storage order, then the size
         --  will have been written with that same order, so byte swap it.

         if Element_Type'Scalar_Storage_Order /= System.Default_Bit_Order then
            Byte_Swap (Rsiz);
         end if;

         --  For a type with discriminants, we have to read into a temporary
         --  buffer if Item is constrained, to check that the discriminants
         --  are correct.

         if Element_Type'Has_Discriminants and then Item'Constrained then
            declare
               RsizS : constant SSE.Storage_Offset :=
                         SSE.Storage_Offset (Rsiz - 1);

               type SA is new SSE.Storage_Array (0 .. RsizS);

               for SA'Alignment use Standard'Maximum_Alignment;
               --  We will perform an unchecked conversion of a pointer-to-SA
               --  into pointer-to-Element_Type. We need to ensure that the
               --  source is always at least as strictly aligned as the target.

               type SAP   is access all SA;
               type ItemP is access all Element_Type;

               pragma Warnings (Off);
               --  We have to turn warnings off for function To_ItemP,
               --  because it gets analyzed for all types, including ones
               --  which can't possibly come this way, and for which the
               --  size of the access types differs.

               function To_ItemP is new Ada.Unchecked_Conversion (SAP, ItemP);

               pragma Warnings (On);

               Buffer : aliased SA;

               pragma Unsuppress (Discriminant_Check);

            begin
               FIO.Read_Buf (AP (File), Buffer'Address, Rsiz);
               Item := To_ItemP (Buffer'Access).all;
               return;
            end;
         end if;

         --  In the case of a non-definite type, make sure the length is OK.
         --  We can't do this in the variant record case, because the size is
         --  based on the current discriminant, so may be apparently wrong.

         if not Element_Type'Has_Discriminants and then Rsiz > Siz then
            raise Program_Error;
         end if;

         FIO.Read_Buf (AP (File), Item'Address, Rsiz);

      --  For definite type without discriminants, use actual size of item

      else
         FIO.Read_Buf (AP (File), Item'Address, Siz);
      end if;
   end Read;

   -----------
   -- Reset --
   -----------

   procedure Reset (File : in out File_Type; Mode : File_Mode) is
   begin
      FIO.Reset (AP (File)'Unrestricted_Access, To_FCB (Mode));
   end Reset;

   procedure Reset (File : in out File_Type) is
   begin
      FIO.Reset (AP (File)'Unrestricted_Access);
   end Reset;

   -----------
   -- Write --
   -----------

   procedure Write (File : File_Type; Item : Element_Type) is
      Siz : constant size_t := (Item'Size + SU - 1) / SU;
      --  Size to be written, in native representation

      Swapped_Siz : size_t := Siz;
      --  Same, possibly byte swapped to account for Element_Type endianness

   begin
      FIO.Check_Write_Status (AP (File));

      --  For non-definite types or types with discriminants, write the size

      if not Element_Type'Definite
        or else Element_Type'Has_Discriminants
      then
         --  If item written has non-default scalar storage order, then the
         --  size is written with that same order, so byte swap it.

         if Element_Type'Scalar_Storage_Order /= System.Default_Bit_Order then
            Byte_Swap (Swapped_Siz);
         end if;

         FIO.Write_Buf
           (AP (File), Swapped_Siz'Address, size_t'Size / System.Storage_Unit);
      end if;

      FIO.Write_Buf (AP (File), Item'Address, Siz);
   end Write;

end Ada.Sequential_IO;
