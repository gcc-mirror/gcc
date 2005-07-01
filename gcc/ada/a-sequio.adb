------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                    A D A . S E Q U E N T I A L _ I O                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 1992-2005 Free Software Foundation, Inc.         --
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

--  This is the generic template for Sequential_IO, i.e. the code that gets
--  duplicated. We absolutely minimize this code by either calling routines
--  in System.File_IO (for common file functions), or in System.Sequential_IO
--  (for specialized Sequential_IO functions)

with Interfaces.C_Streams; use Interfaces.C_Streams;
with System;
with System.CRTL;
with System.File_Control_Block;
with System.File_IO;
with System.Storage_Elements;
with Unchecked_Conversion;

package body Ada.Sequential_IO is

   package FIO renames System.File_IO;
   package FCB renames System.File_Control_Block;
   package SIO renames System.Sequential_IO;
   package SSE renames System.Storage_Elements;

   SU : constant := System.Storage_Unit;

   subtype AP is FCB.AFCB_Ptr;
   subtype FP is SIO.File_Type;

   function To_FCB is new Unchecked_Conversion (File_Mode, FCB.File_Mode);
   function To_SIO is new Unchecked_Conversion (FCB.File_Mode, File_Mode);

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
      Mode : in File_Mode := Out_File;
      Name : in String := "";
      Form : in String := "")
   is
   begin
      SIO.Create (FP (File), To_FCB (Mode), Name, Form);
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
      return FIO.End_Of_File (AP (File));
   end End_Of_File;

   ----------
   -- Form --
   ----------

   function Form (File : in File_Type) return String is
   begin
      return FIO.Form (AP (File));
   end Form;

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
      return To_SIO (FIO.Mode (AP (File)));
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
      SIO.Open (FP (File), To_FCB (Mode), Name, Form);
   end Open;

   ----------
   -- Read --
   ----------

   procedure Read (File : in File_Type; Item : out Element_Type) is
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

         --  For a type with discriminants, we have to read into a temporary
         --  buffer if Item is constrained, to check that the discriminants
         --  are correct.

         pragma Extensions_Allowed (On);
         --  Needed to allow Constrained reference here

         if Element_Type'Has_Discriminants
           and then Item'Constrained
         then
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

               function To_ItemP is new Unchecked_Conversion (SAP, ItemP);

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

   procedure Reset (File : in out File_Type; Mode : in File_Mode) is
   begin
      FIO.Reset (AP (File), To_FCB (Mode));
   end Reset;

   procedure Reset (File : in out File_Type) is
   begin
      FIO.Reset (AP (File));
   end Reset;

   -----------
   -- Write --
   -----------

   procedure Write (File : in File_Type; Item : in Element_Type) is
      Siz : constant size_t := (Item'Size + SU - 1) / SU;

   begin
      FIO.Check_Write_Status (AP (File));

      --  For non-definite types or types with discriminants, write the size

      if not Element_Type'Definite
        or else Element_Type'Has_Discriminants
      then
         FIO.Write_Buf
           (AP (File), Siz'Address, size_t'Size / System.Storage_Unit);
      end if;

      FIO.Write_Buf (AP (File), Item'Address, Siz);
   end Write;

end Ada.Sequential_IO;
