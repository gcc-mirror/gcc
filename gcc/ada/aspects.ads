------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              A S P E C T S                               --
--                                                                          --
--                                 S p e c                                  --
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

--  This package defines the aspects that are recognized in aspect
--  specifications. We separate this off in its own packages to that
--  it can be accessed by the parser without dragging in Sem_Asp

with Namet; use Namet;

package Aspects is

   type Aspect_Id is
     (No_Aspect,                            -- Dummy entry for no aspect
      Aspect_Ada_2005,                      -- GNAT
      Aspect_Ada_2012,                      -- GNAT
      Aspect_Address,
      Aspect_Aliased,
      Aspect_Alignment,
      Aspect_Atomic,
      Aspect_Atomic_Components,
      Aspect_Bit_Order,
      Aspect_C_Pass_By_Copy,
      Aspect_Component_Size,
      Aspect_Discard_Names,
      Aspect_External_Tag,
      Aspect_Favor_Top_Level,               -- GNAT
      Aspect_Inline,
      Aspect_Inline_Always,                 -- GNAT
      Aspect_Invariant,
      Aspect_Machine_Radix,
      Aspect_Object_Size,                   -- GNAT
      Aspect_Pack,
      Aspect_Persistent_BSS,                -- GNAT
      Aspect_Post,
      Aspect_Postcondition,                 -- GNAT (equivalent to Post)
      Aspect_Pre,
      Aspect_Precondition,                  -- GNAT (equivalent to Pre)
      Aspect_Predicate,                     -- GNAT???
      Aspect_Preelaborable_Initialization,
      Aspect_Psect_Object,                  -- GNAT
      Aspect_Pure_Function,                 -- GNAT
      Aspect_Shared,                        -- GNAT (equivalent to Atomic)
      Aspect_Size,
      Aspect_Storage_Pool,
      Aspect_Storage_Size,
      Aspect_Stream_Size,
      Aspect_Suppress,
      Aspect_Suppress_Debug_Info,           -- GNAT
      Aspect_Unchecked_Union,
      Aspect_Universal_Aliasing,            -- GNAT
      Aspect_Unmodified,                    -- GNAT
      Aspect_Unreferenced,                  -- GNAT
      Aspect_Unreferenced_Objects,          -- GNAT
      Aspect_Unsuppress,
      Aspect_Value_Size,                    -- GNAT
      Aspect_Volatile,
      Aspect_Volatile_Components,
      Aspect_Warnings,                      -- GNAT
      Aspect_Weak_External);                -- GNAT

   --  The following array indicates aspects that accept 'Class

   Class_Aspect_OK : constant array (Aspect_Id) of Boolean :=
                       (Aspect_Invariant     => True,
                        Aspect_Pre           => True,
                        Aspect_Precondition  => True,
                        Aspect_Post          => True,
                        Aspect_Postcondition => True,
                        others               => False);

   --  The following type is used for indicating allowed expression forms

   type Aspect_Expression is
     (Optional,               -- Optional boolean expression
      Expression,             -- Required non-boolean expression
      Name);                  -- Required name

   --  The following array indicates what argument type is required

   Aspect_Argument : constant array (Aspect_Id) of Aspect_Expression :=
                       (No_Aspect                           => Optional,
                        Aspect_Ada_2005                     => Optional,
                        Aspect_Ada_2012                     => Optional,
                        Aspect_Address                      => Expression,
                        Aspect_Aliased                      => Optional,
                        Aspect_Alignment                    => Expression,
                        Aspect_Atomic                       => Optional,
                        Aspect_Atomic_Components            => Optional,
                        Aspect_Bit_Order                    => Expression,
                        Aspect_C_Pass_By_Copy               => Optional,
                        Aspect_Component_Size               => Expression,
                        Aspect_Discard_Names                => Optional,
                        Aspect_External_Tag                 => Expression,
                        Aspect_Favor_Top_Level              => Optional,
                        Aspect_Inline                       => Optional,
                        Aspect_Inline_Always                => Optional,
                        Aspect_Invariant                    => Expression,
                        Aspect_Machine_Radix                => Expression,
                        Aspect_Object_Size                  => Expression,
                        Aspect_Pack                         => Optional,
                        Aspect_Persistent_BSS               => Optional,
                        Aspect_Post                         => Expression,
                        Aspect_Postcondition                => Expression,
                        Aspect_Pre                          => Expression,
                        Aspect_Precondition                 => Expression,
                        Aspect_Predicate                    => Expression,
                        Aspect_Preelaborable_Initialization => Optional,
                        Aspect_Psect_Object                 => Optional,
                        Aspect_Pure_Function                => Optional,
                        Aspect_Shared                       => Optional,
                        Aspect_Size                         => Expression,
                        Aspect_Storage_Pool                 => Expression,
                        Aspect_Storage_Size                 => Expression,
                        Aspect_Stream_Size                  => Expression,
                        Aspect_Suppress                     => Name,
                        Aspect_Suppress_Debug_Info          => Optional,
                        Aspect_Unchecked_Union              => Optional,
                        Aspect_Universal_Aliasing           => Optional,
                        Aspect_Unmodified                   => Optional,
                        Aspect_Unreferenced                 => Optional,
                        Aspect_Unreferenced_Objects         => Optional,
                        Aspect_Unsuppress                   => Name,
                        Aspect_Value_Size                   => Expression,
                        Aspect_Volatile                     => Optional,
                        Aspect_Volatile_Components          => Optional,
                        Aspect_Warnings                     => Name,
                        Aspect_Weak_External                => Optional);

   function Get_Aspect_Id (Name : Name_Id) return Aspect_Id;
   --  Given a name Nam, returns the corresponding aspect id value. If the name
   --  does not match any aspect, then No_Aspect is returned as the result.

end Aspects;
