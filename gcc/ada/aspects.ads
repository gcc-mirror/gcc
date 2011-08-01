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

--  This package defines the aspects that are recognized by GNAT in aspect
--  specifications. It also contains the subprograms for storing/retrieving
--  aspect specifications from the tree. The semantic processing for aspect
--  specifications is found in Sem_Ch13.Analyze_Aspect_Specifications.

with Namet; use Namet;
with Types; use Types;

package Aspects is

   --  Type defining recognized aspects

   type Aspect_Id is
     (No_Aspect,                            -- Dummy entry for no aspect
      Aspect_Address,
      Aspect_Alignment,
      Aspect_Bit_Order,
      Aspect_Component_Size,
      Aspect_Dynamic_Predicate,
      Aspect_External_Tag,
      Aspect_Input,
      Aspect_Invariant,
      Aspect_Machine_Radix,
      Aspect_Object_Size,                   -- GNAT
      Aspect_Output,
      Aspect_Post,
      Aspect_Postcondition,
      Aspect_Pre,
      Aspect_Precondition,
      Aspect_Predicate,                     -- GNAT
      Aspect_Read,
      Aspect_Size,
      Aspect_Static_Predicate,
      Aspect_Storage_Pool,
      Aspect_Storage_Size,
      Aspect_Stream_Size,
      Aspect_Suppress,
      Aspect_Type_Invariant,
      Aspect_Unsuppress,
      Aspect_Value_Size,                    -- GNAT
      Aspect_Warnings,
      Aspect_Write,

      --  Remaining aspects have a static boolean value that turns the aspect
      --  on or off. They all correspond to pragmas, and the flag Aspect_Cancel
      --  is set on the pragma if the corresponding aspect is False.

      Aspect_Ada_2005,                      -- GNAT
      Aspect_Ada_2012,                      -- GNAT
      Aspect_Atomic,
      Aspect_Atomic_Components,
      Aspect_Discard_Names,
      Aspect_Favor_Top_Level,               -- GNAT
      Aspect_Inline,
      Aspect_Inline_Always,                 -- GNAT
      Aspect_No_Return,
      Aspect_Pack,
      Aspect_Persistent_BSS,                -- GNAT
      Aspect_Preelaborable_Initialization,
      Aspect_Pure_Function,                 -- GNAT
      Aspect_Shared,                        -- GNAT (equivalent to Atomic)
      Aspect_Suppress_Debug_Info,           -- GNAT
      Aspect_Unchecked_Union,
      Aspect_Universal_Aliasing,            -- GNAT
      Aspect_Unmodified,                    -- GNAT
      Aspect_Unreferenced,                  -- GNAT
      Aspect_Unreferenced_Objects,          -- GNAT
      Aspect_Volatile,
      Aspect_Volatile_Components);

   --  The following array indicates aspects that accept 'Class

   Class_Aspect_OK : constant array (Aspect_Id) of Boolean :=
                       (Aspect_Invariant     => True,
                        Aspect_Pre           => True,
                        Aspect_Predicate     => True,
                        Aspect_Post          => True,
                        others               => False);

   --  The following subtype defines aspects accepting an optional static
   --  boolean parameter indicating if the aspect should be active or
   --  cancelling. If the parameter is missing the effective value is True,
   --  enabling the aspect. If the parameter is present it must be a static
   --  expression of type Standard.Boolean. If the value is True, then the
   --  aspect is enabled. If it is False, the aspect is disabled.

   subtype Boolean_Aspects is
     Aspect_Id range Aspect_Ada_2005 .. Aspect_Id'Last;

   --  The following type is used for indicating allowed expression forms

   type Aspect_Expression is
     (Optional,               -- Optional boolean expression
      Expression,             -- Required non-boolean expression
      Name);                  -- Required name

   --  The following array indicates what argument type is required

   Aspect_Argument : constant array (Aspect_Id) of Aspect_Expression :=
                       (No_Aspect                => Optional,
                        Aspect_Address           => Expression,
                        Aspect_Alignment         => Expression,
                        Aspect_Bit_Order         => Expression,
                        Aspect_Component_Size    => Expression,
                        Aspect_Dynamic_Predicate => Expression,
                        Aspect_External_Tag      => Expression,
                        Aspect_Input             => Name,
                        Aspect_Invariant         => Expression,
                        Aspect_Machine_Radix     => Expression,
                        Aspect_Object_Size       => Expression,
                        Aspect_Output            => Name,
                        Aspect_Post              => Expression,
                        Aspect_Postcondition     => Expression,
                        Aspect_Pre               => Expression,
                        Aspect_Precondition      => Expression,
                        Aspect_Predicate         => Expression,
                        Aspect_Read              => Name,
                        Aspect_Size              => Expression,
                        Aspect_Static_Predicate  => Expression,
                        Aspect_Storage_Pool      => Name,
                        Aspect_Storage_Size      => Expression,
                        Aspect_Stream_Size       => Expression,
                        Aspect_Suppress          => Name,
                        Aspect_Type_Invariant    => Expression,
                        Aspect_Unsuppress        => Name,
                        Aspect_Value_Size        => Expression,
                        Aspect_Warnings          => Name,
                        Aspect_Write             => Name,
                        Boolean_Aspects          => Optional);

   function Get_Aspect_Id (Name : Name_Id) return Aspect_Id;
   pragma Inline (Get_Aspect_Id);
   --  Given a name Nam, returns the corresponding aspect id value. If the name
   --  does not match any aspect, then No_Aspect is returned as the result.

   ---------------------------------------------------
   -- Handling of Aspect Specifications in the Tree --
   ---------------------------------------------------

   --  Several kinds of declaration node permit aspect specifications in Ada
   --  2012 mode. If there was room in all the corresponding declaration nodes,
   --  we could just have a field Aspect_Specifications pointing to a list of
   --  nodes for the aspects (N_Aspect_Specification nodes). But there isn't
   --  room, so we adopt a different approach.

   --  The following subprograms provide access to a specialized interface
   --  implemented internally with a hash table in the body, that provides
   --  access to aspect specifications.

   function Permits_Aspect_Specifications (N : Node_Id) return Boolean;
   --  Returns True if the node N is a declaration node that permits aspect
   --  specifications in the grammar. It is possible for other nodes to have
   --  aspect specifications as a result of Rewrite or Replace calls.

   function Aspect_Specifications (N : Node_Id) return List_Id;
   --  Given a node N, returns the list of N_Aspect_Specification nodes that
   --  are attached to this declaration node. If the node is in the class of
   --  declaration nodes that permit aspect specifications, as defined by the
   --  predicate above, and if their Has_Aspects flag is set to True, then this
   --  will always be a non-empty list. If this flag is set to False, then
   --  No_List is returned. Normally, the only nodes that have Has_Aspects set
   --  True are the nodes for which Permits_Aspect_Specifications would return
   --  True (i.e. the declaration nodes defined in the RM as permitting the
   --  presence of Aspect_Specifications). However, it is possible for the
   --  flag Has_Aspects to be set on other nodes as a result of Rewrite and
   --  Replace calls, and this function may be used to retrieve the aspect
   --  specifications for the original rewritten node in such cases.

   procedure Set_Aspect_Specifications (N : Node_Id; L : List_Id);
   --  The node N must be in the class of declaration nodes that permit aspect
   --  specifications and the Has_Aspects flag must be False on entry. L must
   --  be a non-empty list of N_Aspect_Specification nodes. This procedure sets
   --  the Has_Aspects flag to True, and makes an entry that can be retrieved
   --  by a subsequent Aspect_Specifications call. It is an error to call this
   --  procedure with a node that does not permit aspect specifications, or a
   --  node that has its Has_Aspects flag set True on entry, or with L being an
   --  empty list or No_List.

   procedure Move_Aspects (From : Node_Id; To : Node_Id);
   --  Moves aspects from 'From' node to 'To' node. Has_Aspects (To) must be
   --  False on entry. If Has_Aspects (From) is False, the call has no effect.
   --  Otherwise the aspects are moved and on return Has_Aspects (To) is True,
   --  and Has_Aspects (From) is False.

   function Same_Aspect (A1 : Aspect_Id; A2 : Aspect_Id) return Boolean;
   --  Returns True if A1 and A2 are (essentially) the same aspect. This is not
   --  a simple equality test because e.g. Post and Postcondition are the same.
   --  This is used for detecting duplicate aspects.

   procedure Tree_Write;
   --  Writes contents of Aspect_Specifications hash table to the tree file

   procedure Tree_Read;
   --  Reads contents of Aspect_Specifications hash table from the tree file

end Aspects;
