------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              A S P E C T S                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2010, Free Software Foundation, Inc.            --
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

with Snames; use Snames;

package body Aspects is

   type Aspect_Entry is record
      Nam : Name_Id;
      Asp : Aspect_Id;
   end record;

   Aspect_Names : constant array (Integer range <>) of Aspect_Entry := (
     (Name_Ada_2005,                     Aspect_Ada_2005),
     (Name_Ada_2012,                     Aspect_Ada_2012),
     (Name_Address,                      Aspect_Address),
     (Name_Aliased,                      Aspect_Aliased),
     (Name_Alignment,                    Aspect_Alignment),
     (Name_Atomic,                       Aspect_Atomic),
     (Name_Atomic_Components,            Aspect_Atomic_Components),
     (Name_Bit_Order,                    Aspect_Bit_Order),
     (Name_C_Pass_By_Copy,               Aspect_C_Pass_By_Copy),
     (Name_Component_Size,               Aspect_Component_Size),
     (Name_Discard_Names,                Aspect_Discard_Names),
     (Name_External_Tag,                 Aspect_External_Tag),
     (Name_Favor_Top_Level,              Aspect_Favor_Top_Level),
     (Name_Inline,                       Aspect_Inline),
     (Name_Inline_Always,                Aspect_Inline_Always),
     (Name_Invariant,                    Aspect_Invariant),
     (Name_Machine_Radix,                Aspect_Machine_Radix),
     (Name_Object_Size,                  Aspect_Object_Size),
     (Name_Pack,                         Aspect_Pack),
     (Name_Persistent_BSS,               Aspect_Persistent_BSS),
     (Name_Post,                         Aspect_Post),
     (Name_Postcondition,                Aspect_Postcondition),
     (Name_Pre,                          Aspect_Pre),
     (Name_Precondition,                 Aspect_Precondition),
     (Name_Predicate,                    Aspect_Predicate),
     (Name_Preelaborable_Initialization, Aspect_Preelaborable_Initialization),
     (Name_Psect_Object,                 Aspect_Psect_Object),
     (Name_Pure_Function,                Aspect_Pure_Function),
     (Name_Shared,                       Aspect_Shared),
     (Name_Size,                         Aspect_Size),
     (Name_Storage_Pool,                 Aspect_Storage_Pool),
     (Name_Storage_Size,                 Aspect_Storage_Size),
     (Name_Stream_Size,                  Aspect_Stream_Size),
     (Name_Suppress,                     Aspect_Suppress),
     (Name_Suppress_Debug_Info,          Aspect_Suppress_Debug_Info),
     (Name_Unchecked_Union,              Aspect_Unchecked_Union),
     (Name_Universal_Aliasing,           Aspect_Universal_Aliasing),
     (Name_Unmodified,                   Aspect_Unmodified),
     (Name_Unreferenced,                 Aspect_Unreferenced),
     (Name_Unreferenced_Objects,         Aspect_Unreferenced_Objects),
     (Name_Unsuppress,                   Aspect_Unsuppress),
     (Name_Value_Size,                   Aspect_Value_Size),
     (Name_Volatile,                     Aspect_Volatile),
     (Name_Volatile_Components,          Aspect_Volatile_Components),
     (Name_Warnings,                     Aspect_Warnings),
     (Name_Weak_External,                Aspect_Weak_External));

   -------------------
   -- Get_Aspect_Id --
   -------------------

   function Get_Aspect_Id (Name : Name_Id) return Aspect_Id is
   begin
      for J in Aspect_Names'Range loop
         if Aspect_Names (J).Nam = Name then
            return Aspect_Names (J).Asp;
         end if;
      end loop;

      return No_Aspect;
   end Get_Aspect_Id;

end Aspects;
