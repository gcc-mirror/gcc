------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                 C U D A                                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2010-2023, Free Software Foundation, Inc.         --
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

--  This package defines CUDA-specific datastructures and functions.

with Atree;          use Atree;
with Debug;          use Debug;
with Einfo.Entities; use Einfo.Entities;
with Einfo.Utils;    use Einfo.Utils;
with Elists;         use Elists;
with Errout;         use Errout;
with Nlists;         use Nlists;
with Nmake;          use Nmake;
with Sem_Aux;        use Sem_Aux;
with Sem_Util;       use Sem_Util;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinfo;          use Sinfo;

with GNAT.HTable;

package body GNAT_CUDA is

   --------------------------------------
   -- Hash Table for CUDA_Global nodes --
   --------------------------------------

   type Hash_Range is range 0 .. 510;
   --  Size of hash table headers

   function Hash (F : Entity_Id) return Hash_Range;
   --  Hash function for hash table

   package CUDA_Device_Entities_Table is new
     GNAT.HTable.Simple_HTable
       (Header_Num => Hash_Range,
        Element    => Elist_Id,
        No_Element => No_Elist,
        Key        => Entity_Id,
        Hash       => Hash,
        Equal      => "=");
   --  The keys of this table are package entities whose bodies contain at
   --  least one procedure marked with aspect CUDA_Device. The values are
   --  Elists of the marked entities.

   package CUDA_Kernels_Table is new
     GNAT.HTable.Simple_HTable
       (Header_Num => Hash_Range,
        Element    => Elist_Id,
        No_Element => No_Elist,
        Key        => Entity_Id,
        Hash       => Hash,
        Equal      => "=");
   --  The keys of this table are package entities whose bodies contain at
   --  least one procedure marked with aspect CUDA_Global. The values are
   --  Elists of the marked procedures.

   procedure Empty_CUDA_Global_Subprograms (Pack_Id : Entity_Id);
   --  For all subprograms marked CUDA_Global in Pack_Id, remove declarations
   --  and replace statements with a single null statement.
   --  This is required because CUDA_Global subprograms could be referring to
   --  device-only symbols, which would result in unknown symbols at link time
   --  if kept around.
   --  We choose to empty CUDA_Global subprograms rather than completely
   --  removing them from the package because registering CUDA_Global
   --  subprograms with the CUDA runtime on the host requires knowing the
   --  subprogram's host-side address.

   function Get_CUDA_Device_Entities (Pack_Id : Entity_Id) return Elist_Id;
   --  Returns an Elist of all entities marked with pragma CUDA_Device that
   --  are declared within package body Pack_Body. Returns No_Elist if Pack_Id
   --  does not contain such entities.

   procedure Remove_CUDA_Device_Entities (Pack_Id : Entity_Id);
   --  Removes all entities marked with the CUDA_Device pragma from package
   --  Pack_Id. Must only be called when compiling for the host.

   procedure Set_CUDA_Device_Entities
     (Pack_Id : Entity_Id;
      E       : Elist_Id);
   --  Stores E as the list of CUDA_Device entities belonging to the package
   --  entity Pack_Id. Pack_Id must not have a list of device entities.

   procedure Set_CUDA_Kernels
     (Pack_Id : Entity_Id;
      Kernels : Elist_Id);
   --  Stores Kernels as the list of kernels belonging to the package entity
   --  Pack_Id. Pack_Id must not have a list of kernels.

   ----------------------------
   -- Add_CUDA_Device_Entity --
   ----------------------------

   procedure Add_CUDA_Device_Entity
     (Pack_Id : Entity_Id;
      E       : Entity_Id)
   is
      Device_Entities : Elist_Id := Get_CUDA_Device_Entities (Pack_Id);
   begin
      if No (Device_Entities) then
         Device_Entities := New_Elmt_List;
         Set_CUDA_Device_Entities (Pack_Id, Device_Entities);
      end if;
      Append_Elmt (E, Device_Entities);
   end Add_CUDA_Device_Entity;

   ---------------------
   -- Add_CUDA_Kernel --
   ---------------------

   procedure Add_CUDA_Kernel
     (Pack_Id : Entity_Id;
      Kernel  : Entity_Id)
   is
      Kernels : Elist_Id := Get_CUDA_Kernels (Pack_Id);
   begin
      if No (Kernels) then
         Kernels := New_Elmt_List;
         Set_CUDA_Kernels (Pack_Id, Kernels);
      end if;
      Append_Elmt (Kernel, Kernels);
   end Add_CUDA_Kernel;

   -----------------------------------
   -- Empty_CUDA_Global_Subprograms --
   -----------------------------------

   procedure Empty_CUDA_Global_Subprograms (Pack_Id : Entity_Id) is
      Spec_Id     : constant Node_Id := Corresponding_Spec (Pack_Id);
      Kernels     : constant Elist_Id := Get_CUDA_Kernels (Spec_Id);
      Kernel_Elm  : Elmt_Id;
      Kernel      : Entity_Id;
      Kernel_Body : Node_Id;
      Null_Body   : Entity_Id;
      Loc         : Source_Ptr;
   begin
      --  It is an error to empty CUDA_Global subprograms when not compiling
      --  for the host.
      pragma Assert (Debug_Flag_Underscore_C);

      if No (Kernels) then
         return;
      end if;

      Kernel_Elm := First_Elmt (Kernels);
      while Present (Kernel_Elm) loop
         Kernel      := Node (Kernel_Elm);
         Kernel_Body := Subprogram_Body (Kernel);
         Loc         := Sloc (Kernel_Body);

         Null_Body := Make_Subprogram_Body (Loc,
           Specification              => Specification (Kernel_Body),
           Declarations               => New_List,
           Handled_Statement_Sequence =>
             Make_Handled_Sequence_Of_Statements (Loc,
               Statements => New_List (Make_Null_Statement (Loc))));

         Set_Corresponding_Spec (Null_Body,
           Corresponding_Spec (Kernel_Body));

         Rewrite (Kernel_Body, Null_Body);

         Next_Elmt (Kernel_Elm);
      end loop;
   end Empty_CUDA_Global_Subprograms;

   -------------------------
   -- Expand_CUDA_Package --
   -------------------------

   procedure Expand_CUDA_Package (N : Node_Id) is
   begin

      --  If not compiling for the host, do not do anything.

      if not Debug_Flag_Underscore_C then
         return;
      end if;

      --  Remove the content (both declarations and statements) of CUDA_Global
      --  procedures. This is required because CUDA_Global functions could be
      --  referencing entities available only on the device, which would result
      --  in unknown symbol errors at link time.

      Empty_CUDA_Global_Subprograms (N);

      --  Remove CUDA_Device entities (except if they are also CUDA_Host), as
      --  they can only be referenced from the device and might reference
      --  device-only symbols.

      Remove_CUDA_Device_Entities
        (Package_Specification (Corresponding_Spec (N)));
   end Expand_CUDA_Package;

   ----------
   -- Hash --
   ----------

   function Hash (F : Entity_Id) return Hash_Range is
   begin
      return Hash_Range (F mod 511);
   end Hash;

   ------------------------------
   -- Get_CUDA_Device_Entities --
   ------------------------------

   function Get_CUDA_Device_Entities (Pack_Id : Entity_Id) return Elist_Id is
   begin
      return CUDA_Device_Entities_Table.Get (Pack_Id);
   end Get_CUDA_Device_Entities;

   ----------------------
   -- Get_CUDA_Kernels --
   ----------------------

   function Get_CUDA_Kernels (Pack_Id : Entity_Id) return Elist_Id is
   begin
      return CUDA_Kernels_Table.Get (Pack_Id);
   end Get_CUDA_Kernels;

   ---------------------------------
   -- Remove_CUDA_Device_Entities --
   ---------------------------------

   procedure Remove_CUDA_Device_Entities (Pack_Id : Entity_Id) is
      Device_Entities : constant Elist_Id :=
        Get_CUDA_Device_Entities (Pack_Id);
      Device_Elmt     : Elmt_Id;
      Device_Entity   : Entity_Id;
      Bod             : Node_Id;
   begin
      pragma Assert (Debug_Flag_Underscore_C);

      if No (Device_Entities) then
         return;
      end if;

      Device_Elmt := First_Elmt (Device_Entities);
      while Present (Device_Elmt) loop
         Device_Entity := Node (Device_Elmt);
         Next_Elmt (Device_Elmt);

         case Ekind (Device_Entity) is
            when E_Function | E_Procedure =>
               Bod := Subprogram_Body (Device_Entity);

               if Nkind (Parent (Bod)) = N_Subunit
                 and then Present (Corresponding_Stub (Parent (Bod)))
               then
                  Error_Msg_N
                    ("Cuda_Device not suported on separate subprograms",
                     Corresponding_Stub (Parent (Bod)));
               else
                  Remove (Bod);
                  Remove (Subprogram_Spec (Device_Entity));
               end if;

            when E_Variable | E_Constant =>
               Remove (Declaration_Node (Device_Entity));

            when others =>
               pragma Assert (False);
         end case;

         Remove_Entity_And_Homonym (Device_Entity);
      end loop;
   end Remove_CUDA_Device_Entities;

   ------------------------------
   -- Set_CUDA_Device_Entities --
   ------------------------------

   procedure Set_CUDA_Device_Entities
     (Pack_Id : Entity_Id;
      E       : Elist_Id)
   is
   begin
      pragma Assert (No (Get_CUDA_Device_Entities (Pack_Id)));
      CUDA_Device_Entities_Table.Set (Pack_Id, E);
   end Set_CUDA_Device_Entities;

   ----------------------
   -- Set_CUDA_Kernels --
   ----------------------

   procedure Set_CUDA_Kernels
     (Pack_Id : Entity_Id;
      Kernels : Elist_Id)
   is
   begin
      pragma Assert (No (Get_CUDA_Kernels (Pack_Id)));
      CUDA_Kernels_Table.Set (Pack_Id, Kernels);
   end Set_CUDA_Kernels;

end GNAT_CUDA;
