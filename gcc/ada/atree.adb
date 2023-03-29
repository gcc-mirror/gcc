------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                A T R E E                                 --
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
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with Aspects;        use Aspects;
with Debug;          use Debug;
with Namet;          use Namet;
with Nlists;         use Nlists;
with Opt;            use Opt;
with Output;         use Output;
with Sinfo.Utils;    use Sinfo.Utils;
with System.Storage_Elements;

package body Atree is

   ---------------
   -- Debugging --
   ---------------

   --  Suppose you find that node 12345 is messed up. You might want to find
   --  the code that created that node. See sinfo-utils.adb for how to do that.

   Ignored_Ghost_Recording_Proc : Ignored_Ghost_Record_Proc := null;
   --  This soft link captures the procedure invoked during the creation of an
   --  ignored Ghost node or entity.

   Locked : Boolean := False;
   --  Compiling with assertions enabled, node contents modifications are
   --  permitted only when this switch is set to False; compiling without
   --  assertions this lock has no effect.

   Reporting_Proc : Report_Proc := null;
   --  Set_Reporting_Proc sets this. Set_Reporting_Proc must be called only
   --  once.

   Rewriting_Proc : Rewrite_Proc := null;
   --  This soft link captures the procedure invoked during a node rewrite

   -----------------------------
   -- Local Objects and Types --
   -----------------------------

   Comes_From_Source_Default : Boolean := False;

   use Atree_Private_Part;
   --  We are also allowed to see our private data structures

   --------------------------------------------------
   -- Implementation of Tree Substitution Routines --
   --------------------------------------------------

   --  A separate table keeps track of the mapping between rewritten nodes and
   --  their corresponding original tree nodes. Rewrite makes an entry in this
   --  table for use by Original_Node. By default the entry in this table
   --  points to the original unwritten node. Note that if a node is rewritten
   --  more than once, there is no easy way to get to the intermediate
   --  rewrites; the node itself is the latest version, and the entry in this
   --  table is the original.

   --  Note: This could be a node field.

   package Orig_Nodes is new Table.Table (
      Table_Component_Type => Node_Id,
      Table_Index_Type     => Node_Id'Base,
      Table_Low_Bound      => First_Node_Id,
      Table_Initial        => Alloc.Node_Offsets_Initial,
      Table_Increment      => Alloc.Node_Offsets_Increment,
      Table_Name           => "Orig_Nodes");

   ------------------
   -- Parent Stack --
   ------------------

   --  A separate table is used to traverse trees. It passes the parent field
   --  of each node to the called process subprogram. It is defined global to
   --  avoid adding performance overhead if allocated each time the traversal
   --  functions are invoked.

   package Parents_Stack is new Table.Table
     (Table_Component_Type => Node_Id,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 256,
      Table_Increment      => 100,
      Table_Name           => "Parents_Stack");

   --------------------------
   -- Paren_Count Handling --
   --------------------------

   --  The Small_Paren_Count field has range 0 .. 3. If the Paren_Count is
   --  in the range 0 .. 2, then it is stored as Small_Paren_Count. Otherwise,
   --  Small_Paren_Count = 3, and the actual Paren_Count is stored in the
   --  Paren_Counts table.
   --
   --  We use linear search on the Paren_Counts table, which is plenty
   --  efficient because only pathological programs will use it. Nobody
   --  writes (((X + Y))).

   type Paren_Count_Entry is record
      Nod : Node_Id;
      --  The node to which this count applies

      Count : Nat range 3 .. Nat'Last;
      --  The count of parentheses, which will be in the indicated range
   end record;

   package Paren_Counts is new Table.Table (
     Table_Component_Type => Paren_Count_Entry,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 0,
     Table_Initial        => 10,
     Table_Increment      => 200,
     Table_Name           => "Paren_Counts");

   procedure Set_Paren_Count_Of_Copy (Target, Source : Node_Id);
   pragma Inline (Set_Paren_Count_Of_Copy);
   --  Called when copying a node. Makes sure the Paren_Count of the copy is
   --  correct.

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Allocate_New_Node (Kind : Node_Kind) return Node_Id;
   pragma Inline (Allocate_New_Node);
   --  Allocate a new node or first part of a node extension. Initialize the
   --  Nodes.Table entry, Flags, Orig_Nodes, and List tables.

   procedure Fix_Parents (Ref_Node, Fix_Node : Node_Id);
   --  Fix up parent pointers for the children of Fix_Node after a copy,
   --  setting them to Fix_Node when they pointed to Ref_Node.

   generic
      with function Process
        (Parent_Node : Node_Id;
         Node        : Node_Id) return Traverse_Result is <>;
   function Internal_Traverse_With_Parent
     (Node : Node_Id) return Traverse_Final_Result;
   pragma Inline (Internal_Traverse_With_Parent);
   --  Internal function that provides a functionality similar to Traverse_Func
   --  but extended to pass the Parent node to the called Process subprogram;
   --  delegates to Traverse_Func_With_Parent the initialization of the stack
   --  data structure which stores the parent nodes (cf. Parents_Stack).
   --  ??? Could we factorize the common code of Internal_Traverse_Func and
   --  Traverse_Func?

   procedure Mark_New_Ghost_Node (N : Node_Or_Entity_Id);
   --  Mark arbitrary node or entity N as Ghost when it is created within a
   --  Ghost region.

   procedure Report (Target, Source : Node_Id);
   pragma Inline (Report);
   --  Invoke the reporting procedure if available

   function Size_In_Slots (N : Node_Or_Entity_Id) return Slot_Count;
   --  Number of slots belonging to N. This can be less than
   --  Size_In_Slots_To_Alloc for entities. Includes both header
   --  and dynamic slots.

   function Size_In_Slots_Dynamic (N : Node_Or_Entity_Id) return Slot_Count;
   --  Just counts the number of dynamic slots

   function Size_In_Slots_To_Alloc (N : Node_Or_Entity_Id) return Slot_Count;
   function Size_In_Slots_To_Alloc (Kind : Node_Kind) return Slot_Count;
   --  Number of slots to allocate for a node or entity. For entities, we have
   --  to allocate the max, because we don't know the Ekind when this is
   --  called.

   function Off_F (N : Node_Id) return Node_Offset with Inline;
   --  Offset of the first dynamic slot of N in Slots.Table.
   --  The actual offset of this slot from the start of the node
   --  is not 0; this is logically the first slot after the header
   --  slots.

   function Off_0 (N : Node_Id) return Node_Offset'Base with Inline;
   --  This is for zero-origin addressing of the dynamic slots.
   --  It points to slot 0 of N in Slots.Table, which does not exist,
   --  because the first few slots are stored in the header.

   function Off_L (N : Node_Id) return Node_Offset with Inline;
   --  Offset of the last slot of N in Slots.Table

   procedure Zero_Dynamic_Slots (First, Last : Node_Offset'Base) with Inline;
   --  Set dynamic slots in the range First..Last to zero

   procedure Zero_Header_Slots (N : Node_Or_Entity_Id) with Inline;
   --  Zero the header slots belonging to N

   procedure Zero_Slots (N : Node_Or_Entity_Id) with Inline;
   --  Zero the slots belonging to N (both header and dynamic)

   procedure Copy_Dynamic_Slots
     (From, To : Node_Offset; Num_Slots : Slot_Count)
     with Inline;
   --  Copy Num_Slots slots from From to To. Caller is responsible for ensuring
   --  that the Num_Slots at To are a reasonable place to copy to.

   procedure Copy_Slots (Source, Destination : Node_Id) with Inline;
   --  Copies the slots (both header and dynamic) of Source to Destination;
   --  uses the node kind to determine the Num_Slots.

   function Get_Field_Value
     (N : Node_Id; Field : Node_Or_Entity_Field) return Field_Size_32_Bit;
   --  Get any field value as a Field_Size_32_Bit. If the field is smaller than
   --  32 bits, convert it to Field_Size_32_Bit. The Field must be present in
   --  the Nkind of N.

   procedure Set_Field_Value
     (N : Node_Id; Field : Node_Or_Entity_Field; Val : Field_Size_32_Bit);
   --  Set any field value as a Field_Size_32_Bit. If the field is smaller than
   --  32 bits, convert it from Field_Size_32_Bit, and Val had better be small
   --  enough. The Field must be present in the Nkind of N.

   procedure Check_Vanishing_Fields
     (Old_N : Node_Id; New_Kind : Node_Kind);
   --  Called whenever Nkind is modified. Raises an exception if not all
   --  vanishing fields are in their initial zero state.

   procedure Check_Vanishing_Fields
     (Old_N : Entity_Id; New_Kind : Entity_Kind);
   --  Above are the same as the ones for nodes, but for entities

   procedure Init_Nkind (N : Node_Id; Val : Node_Kind);
   --  Initialize the Nkind field, which must not have been set already. This
   --  cannot be used to modify an already-initialized Nkind field. See also
   --  Mutate_Nkind.

   procedure Mutate_Nkind
     (N : Node_Id; Val : Node_Kind; Old_Size : Slot_Count);
   --  Called by the other Mutate_Nkind to do all the work. This is needed
   --  because the call in Change_Node, which calls this one directly, happens
   --  after zeroing N's slots, which destroys its Nkind, which prevents us
   --  from properly computing Old_Size.

   package Field_Checking is
      --  Functions for checking field access, used only in assertions

      function Field_Present
        (Kind : Node_Kind; Field : Node_Field) return Boolean;
      function Field_Present
        (Kind : Entity_Kind; Field : Entity_Field) return Boolean;
      --  True if a node/entity of the given Kind has the given Field.
      --  Always True if assertions are disabled.

   end Field_Checking;

   package body Field_Checking is

      --  Tables used by Field_Present

      type Node_Field_Sets is array (Node_Kind) of Node_Field_Set;
      type Node_Field_Sets_Ptr is access all Node_Field_Sets;
      Node_Fields_Present : Node_Field_Sets_Ptr;

      type Entity_Field_Sets is array (Entity_Kind) of Entity_Field_Set;
      type Entity_Field_Sets_Ptr is access all Entity_Field_Sets;
      Entity_Fields_Present : Entity_Field_Sets_Ptr;

      procedure Init_Tables;

      function Create_Node_Fields_Present
        (Kind : Node_Kind) return Node_Field_Set;
      function Create_Entity_Fields_Present
        (Kind : Entity_Kind) return Entity_Field_Set;
      --  Computes the set of fields present in each Node/Entity Kind. Used to
      --  initialize the above tables.

      --------------------------------
      -- Create_Node_Fields_Present --
      --------------------------------

      function Create_Node_Fields_Present
        (Kind : Node_Kind) return Node_Field_Set
      is
         Result : Node_Field_Set := (others => False);
      begin
         for J in Node_Field_Table (Kind)'Range loop
            Result (Node_Field_Table (Kind) (J)) := True;
         end loop;

         return Result;
      end Create_Node_Fields_Present;

      --------------------------------
      -- Create_Entity_Fields_Present --
      --------------------------------

      function Create_Entity_Fields_Present
        (Kind : Entity_Kind) return Entity_Field_Set
      is
         Result : Entity_Field_Set := (others => False);
      begin
         for J in Entity_Field_Table (Kind)'Range loop
            Result (Entity_Field_Table (Kind) (J)) := True;
         end loop;

         return Result;
      end Create_Entity_Fields_Present;

      -----------------
      -- Init_Tables --
      -----------------

      procedure Init_Tables is
      begin
         Node_Fields_Present := new Node_Field_Sets;

         for Kind in Node_Kind loop
            Node_Fields_Present (Kind) := Create_Node_Fields_Present (Kind);
         end loop;

         Entity_Fields_Present := new Entity_Field_Sets;

         for Kind in Entity_Kind loop
            Entity_Fields_Present (Kind) :=
              Create_Entity_Fields_Present (Kind);
         end loop;
      end Init_Tables;

      --  In production mode, we leave Node_Fields_Present and
      --  Entity_Fields_Present null. Field_Present is only for
      --  use in assertions.

      pragma Debug (Init_Tables);

      function Field_Present
        (Kind : Node_Kind; Field : Node_Field) return Boolean is
      begin
         if Node_Fields_Present = null then
            return True;
         end if;

         return Node_Fields_Present (Kind) (Field);
      end Field_Present;

      function Field_Present
        (Kind : Entity_Kind; Field : Entity_Field) return Boolean is
      begin
         if Entity_Fields_Present = null then
            return True;
         end if;

         return Entity_Fields_Present (Kind) (Field);
      end Field_Present;

   end Field_Checking;

   ------------------------
   -- Atree_Private_Part --
   ------------------------

   package body Atree_Private_Part is

      --  The following validators are disabled in production builds, by being
      --  called in pragma Debug. They are also disabled by default in debug
      --  builds, by setting the flags below, because they make the compiler
      --  very slow (10 to 20 times slower). Validate can be set True to debug
      --  the low-level accessors.
      --
      --  Even if Validate is True, validation is disabled during
      --  Validate_... calls to prevent infinite recursion
      --  (Validate_... procedures call field getters, which call
      --  Validate_... procedures). That's what the Enable_Validate_...
      --  flags are for; they are toggled so that when we're inside one
      --  of them, and enter it again, the inner call doesn't do anything.
      --  These flags are irrelevant when Validate is False.

      Validate : constant Boolean := False;

      Enable_Validate_Node,
      Enable_Validate_Node_Write,
      Enable_Validate_Node_And_Offset,
      Enable_Validate_Node_And_Offset_Write :
        Boolean := Validate;

      procedure Validate_Node_And_Offset
        (N : Node_Or_Entity_Id; Offset : Field_Offset);
      procedure Validate_Node_And_Offset_Write
        (N : Node_Or_Entity_Id; Offset : Field_Offset);
      --  Asserts N is OK, and the Offset in slots is within N. Note that this
      --  does not guarantee that the offset is valid, just that it's not past
      --  the last slot. It could be pointing at unused bits within the node,
      --  or unused padding at the end. The "_Write" version is used when we're
      --  about to modify the node.

      procedure Validate_Node_And_Offset
        (N : Node_Or_Entity_Id; Offset : Field_Offset) is
      begin
         if Enable_Validate_Node_And_Offset then
            Enable_Validate_Node_And_Offset := False;

            pragma Debug (Validate_Node (N));
            pragma Assert (Offset'Valid);
            pragma Assert (Offset < Size_In_Slots (N));

            Enable_Validate_Node_And_Offset := True;
         end if;
      end Validate_Node_And_Offset;

      procedure Validate_Node_And_Offset_Write
        (N : Node_Or_Entity_Id; Offset : Field_Offset) is
      begin
         if Enable_Validate_Node_And_Offset_Write then
            Enable_Validate_Node_And_Offset_Write := False;

            pragma Debug (Validate_Node_Write (N));
            pragma Assert (Offset'Valid);
            pragma Assert (Offset < Size_In_Slots (N));

            Enable_Validate_Node_And_Offset_Write := True;
         end if;
      end Validate_Node_And_Offset_Write;

      procedure Validate_Node (N : Node_Or_Entity_Id) is
      begin
         if Enable_Validate_Node then
            Enable_Validate_Node := False;

            pragma Assert (N'Valid);
            pragma Assert (N <= Node_Offsets.Last);
            pragma Assert (Off_L (N) >= Off_0 (N));
            pragma Assert (Off_L (N) >= Off_F (N) - 1);
            pragma Assert (Off_L (N) <= Slots.Last);
            pragma Assert (Nkind (N)'Valid);
            pragma Assert (Nkind (N) /= N_Unused_At_End);

            if Nkind (N) in N_Entity then
               pragma Assert (Ekind (N)'Valid);
            end if;

            if Nkind (N) in
                N_Aggregate
              | N_Attribute_Definition_Clause
              | N_Aspect_Specification
              | N_Extension_Aggregate
              | N_Freeze_Entity
              | N_Freeze_Generic_Entity
              | N_Has_Entity
              | N_Selected_Component
              | N_Use_Package_Clause
            then
               pragma Assert (Entity_Or_Associated_Node (N)'Valid);
            end if;

            Enable_Validate_Node := True;
         end if;
      end Validate_Node;

      procedure Validate_Node_Write (N : Node_Or_Entity_Id) is
      begin
         if Enable_Validate_Node_Write then
            Enable_Validate_Node_Write := False;

            pragma Debug (Validate_Node (N));
            pragma Assert (not Locked);

            Enable_Validate_Node_Write := True;
         end if;
      end Validate_Node_Write;

      function Is_Valid_Node (U : Union_Id) return Boolean is
      begin
         return Node_Id'Base (U) <= Node_Offsets.Last;
      end Is_Valid_Node;

      function Alloc_Node_Id return Node_Id is
      begin
         Node_Offsets.Increment_Last;
         return Node_Offsets.Last;
      end Alloc_Node_Id;

      function Alloc_Slots (Num_Slots : Slot_Count) return Node_Offset is
      begin
         return Result : constant Node_Offset := Slots.Last + 1 do
            Slots.Set_Last (Slots.Last + Num_Slots);
         end return;
      end Alloc_Slots;

      function Get_1_Bit_Field
        (N : Node_Or_Entity_Id; Offset : Field_Offset) return Field_Type
      is
         pragma Assert (Field_Type'Size = 1);

         function Cast is new
           Ada.Unchecked_Conversion (Field_Size_1_Bit, Field_Type);
         Val : constant Field_Size_1_Bit := Get_1_Bit_Val (N, Offset);
      begin
         return Cast (Val);
      end Get_1_Bit_Field;

      function Get_2_Bit_Field
        (N : Node_Or_Entity_Id; Offset : Field_Offset) return Field_Type
      is
         pragma Assert (Field_Type'Size = 2);

         function Cast is new
           Ada.Unchecked_Conversion (Field_Size_2_Bit, Field_Type);
         Val : constant Field_Size_2_Bit := Get_2_Bit_Val (N, Offset);
      begin
         return Cast (Val);
      end Get_2_Bit_Field;

      function Get_4_Bit_Field
        (N : Node_Or_Entity_Id; Offset : Field_Offset) return Field_Type
      is
         pragma Assert (Field_Type'Size = 4);

         function Cast is new
           Ada.Unchecked_Conversion (Field_Size_4_Bit, Field_Type);
         Val : constant Field_Size_4_Bit := Get_4_Bit_Val (N, Offset);
      begin
         return Cast (Val);
      end Get_4_Bit_Field;

      function Get_8_Bit_Field
        (N : Node_Or_Entity_Id; Offset : Field_Offset) return Field_Type
      is
         pragma Assert (Field_Type'Size = 8);

         function Cast is new
           Ada.Unchecked_Conversion (Field_Size_8_Bit, Field_Type);
         Val : constant Field_Size_8_Bit := Get_8_Bit_Val (N, Offset);
      begin
         return Cast (Val);
      end Get_8_Bit_Field;

      function Get_32_Bit_Field
        (N : Node_Or_Entity_Id; Offset : Field_Offset) return Field_Type
      is
         pragma Assert (Field_Type'Size = 32);

         function Cast is new
           Ada.Unchecked_Conversion (Field_Size_32_Bit, Field_Type);

         Val : constant Field_Size_32_Bit := Get_32_Bit_Val (N, Offset);
         Result : constant Field_Type := Cast (Val);
         --  Note: declaring Result here instead of directly returning
         --  Cast (...) helps CodePeer understand that there are no issues
         --  around uninitialized variables.
      begin
         return Result;
      end Get_32_Bit_Field;

      function Get_32_Bit_Field_With_Default
        (N : Node_Or_Entity_Id; Offset : Field_Offset) return Field_Type
      is
         function Get_Field is new Get_32_Bit_Field (Field_Type) with Inline;
         Result : Field_Type;
      begin
         --  If the field has not yet been set, it will be equal to zero.
         --  That is of the "wrong" type, so we fetch it as a
         --  Field_Size_32_Bit.

         if Get_32_Bit_Val (N, Offset) = 0 then
            Result := Default_Val;

         else
            Result := Get_Field (N, Offset);
         end if;

         return Result;
      end Get_32_Bit_Field_With_Default;

      function Get_Valid_32_Bit_Field
        (N : Node_Or_Entity_Id; Offset : Field_Offset) return Field_Type
      is
         pragma Assert (Get_32_Bit_Val (N, Offset) /= 0);
         --  If the field has not yet been set, it will be equal to zero.
         --  This asserts that we don't call Get_ before Set_. Note that
         --  the predicate on the Val parameter of Set_ checks for the No_...
         --  value, so it can't possibly be (for example) No_Uint here.

         function Get_Field is new Get_32_Bit_Field (Field_Type) with Inline;
         Result : constant Field_Type := Get_Field (N, Offset);
      begin
         return Result;
      end Get_Valid_32_Bit_Field;

      procedure Set_1_Bit_Field
        (N : Node_Or_Entity_Id; Offset : Field_Offset; Val : Field_Type)
      is
         pragma Assert (Field_Type'Size = 1);

         function Cast is new
           Ada.Unchecked_Conversion (Field_Type, Field_Size_1_Bit);
      begin
         Set_1_Bit_Val (N, Offset, Cast (Val));
      end Set_1_Bit_Field;

      procedure Set_2_Bit_Field
        (N : Node_Or_Entity_Id; Offset : Field_Offset; Val : Field_Type)
      is
         pragma Assert (Field_Type'Size = 2);

         function Cast is new
           Ada.Unchecked_Conversion (Field_Type, Field_Size_2_Bit);
      begin
         Set_2_Bit_Val (N, Offset, Cast (Val));
      end Set_2_Bit_Field;

      procedure Set_4_Bit_Field
        (N : Node_Or_Entity_Id; Offset : Field_Offset; Val : Field_Type)
      is
         pragma Assert (Field_Type'Size = 4);

         function Cast is new
           Ada.Unchecked_Conversion (Field_Type, Field_Size_4_Bit);
      begin
         Set_4_Bit_Val (N, Offset, Cast (Val));
      end Set_4_Bit_Field;

      procedure Set_8_Bit_Field
        (N : Node_Or_Entity_Id; Offset : Field_Offset; Val : Field_Type)
      is
         pragma Assert (Field_Type'Size = 8);

         function Cast is new
           Ada.Unchecked_Conversion (Field_Type, Field_Size_8_Bit);
      begin
         Set_8_Bit_Val (N, Offset, Cast (Val));
      end Set_8_Bit_Field;

      procedure Set_32_Bit_Field
        (N : Node_Or_Entity_Id; Offset : Field_Offset; Val : Field_Type)
      is
         pragma Assert (Field_Type'Size = 32);

         function Cast is new
           Ada.Unchecked_Conversion (Field_Type, Field_Size_32_Bit);
      begin
         Set_32_Bit_Val (N, Offset, Cast (Val));
      end Set_32_Bit_Field;

      pragma Style_Checks ("M90");

      -----------------------------------
      -- Low-level getters and setters --
      -----------------------------------

      --  In the getters and setters below, we use shifting and masking to
      --  simulate packed arrays. F_Size is the field size in bits. Mask is
      --  that number of 1 bits in the low-order bits. F_Per_Slot is the number
      --  of fields per slot. Slot_Off is the offset of the slot of interest.
      --  S is the slot at that offset. V is the amount to shift by.

      function In_NH (Slot_Off : Field_Offset) return Boolean is
        (Slot_Off < N_Head);
      --  In_NH stands for "in Node_Header", not "in New Hampshire"

      function Get_Slot
        (N : Node_Or_Entity_Id; Slot_Off : Field_Offset)
         return Slot is
         (if In_NH (Slot_Off) then
            Node_Offsets.Table (N).Slots (Slot_Off)
          else Slots.Table (Node_Offsets.Table (N).Offset + Slot_Off));
      --  Get the slot value, either directly from the node header, or
      --  indirectly from the Slots table.

      procedure Set_Slot
        (N : Node_Or_Entity_Id; Slot_Off : Field_Offset; S : Slot);
      --  Set the slot value, either directly from the node header, or
      --  indirectly from the Slots table, to S.

      function Get_1_Bit_Val
        (N : Node_Or_Entity_Id; Offset : Field_Offset) return Field_Size_1_Bit
      is
         F_Size : constant := 1;
         Mask : constant := 2**F_Size - 1;
         F_Per_Slot : constant Field_Offset := Slot_Size / F_Size;
         Slot_Off : constant Field_Offset := Offset / F_Per_Slot;
         S : constant Slot := Get_Slot (N, Slot_Off);
         V : constant Natural := Natural ((Offset mod F_Per_Slot) * F_Size);
         pragma Debug (Validate_Node_And_Offset (N, Slot_Off));
         Raw : constant Field_Size_1_Bit :=
           Field_Size_1_Bit (Shift_Right (S, V) and Mask);
      begin
         return Raw;
      end Get_1_Bit_Val;

      function Get_2_Bit_Val
        (N : Node_Or_Entity_Id; Offset : Field_Offset) return Field_Size_2_Bit
      is
         F_Size : constant := 2;
         Mask : constant := 2**F_Size - 1;
         F_Per_Slot : constant Field_Offset := Slot_Size / F_Size;
         Slot_Off : constant Field_Offset := Offset / F_Per_Slot;
         S : constant Slot := Get_Slot (N, Slot_Off);
         V : constant Natural := Natural ((Offset mod F_Per_Slot) * F_Size);
         pragma Debug (Validate_Node_And_Offset (N, Slot_Off));
         Raw : constant Field_Size_2_Bit :=
           Field_Size_2_Bit (Shift_Right (S, V) and Mask);
      begin
         return Raw;
      end Get_2_Bit_Val;

      function Get_4_Bit_Val
        (N : Node_Or_Entity_Id; Offset : Field_Offset) return Field_Size_4_Bit
      is
         F_Size : constant := 4;
         Mask : constant := 2**F_Size - 1;
         F_Per_Slot : constant Field_Offset := Slot_Size / F_Size;
         Slot_Off : constant Field_Offset := Offset / F_Per_Slot;
         S : constant Slot := Get_Slot (N, Slot_Off);
         V : constant Natural := Natural ((Offset mod F_Per_Slot) * F_Size);
         pragma Debug (Validate_Node_And_Offset (N, Slot_Off));
         Raw : constant Field_Size_4_Bit :=
           Field_Size_4_Bit (Shift_Right (S, V) and Mask);
      begin
         return Raw;
      end Get_4_Bit_Val;

      function Get_8_Bit_Val
        (N : Node_Or_Entity_Id; Offset : Field_Offset) return Field_Size_8_Bit
      is
         F_Size : constant := 8;
         Mask : constant := 2**F_Size - 1;
         F_Per_Slot : constant Field_Offset := Slot_Size / F_Size;
         Slot_Off : constant Field_Offset := Offset / F_Per_Slot;
         S : constant Slot := Get_Slot (N, Slot_Off);
         V : constant Natural := Natural ((Offset mod F_Per_Slot) * F_Size);
         pragma Debug (Validate_Node_And_Offset (N, Slot_Off));
         Raw : constant Field_Size_8_Bit :=
           Field_Size_8_Bit (Shift_Right (S, V) and Mask);
      begin
         return Raw;
      end Get_8_Bit_Val;

      function Get_32_Bit_Val
        (N : Node_Or_Entity_Id; Offset : Field_Offset) return Field_Size_32_Bit
      is
         F_Size : constant := 32;
         --  No Mask needed
         F_Per_Slot : constant Field_Offset := Slot_Size / F_Size;
         Slot_Off : constant Field_Offset := Offset / F_Per_Slot;
         S : constant Slot := Get_Slot (N, Slot_Off);
         pragma Debug (Validate_Node_And_Offset (N, Slot_Off));
         Raw : constant Field_Size_32_Bit :=
           Field_Size_32_Bit (S);
      begin
         return Raw;
      end Get_32_Bit_Val;

      procedure Set_Slot
        (N : Node_Or_Entity_Id; Slot_Off : Field_Offset; S : Slot) is
      begin
         if In_NH (Slot_Off) then
            Node_Offsets.Table (N).Slots (Slot_Off) := S;
         else
            Slots.Table (Node_Offsets.Table (N).Offset + Slot_Off) := S;
         end if;
      end Set_Slot;

      procedure Set_1_Bit_Val
        (N : Node_Or_Entity_Id; Offset : Field_Offset; Val : Field_Size_1_Bit)
      is
         F_Size : constant := 1;
         Mask : constant := 2**F_Size - 1;
         F_Per_Slot : constant Field_Offset := Slot_Size / F_Size;
         Slot_Off : constant Field_Offset := Offset / F_Per_Slot;
         S : constant Slot := Get_Slot (N, Slot_Off);
         V : constant Natural := Natural ((Offset mod F_Per_Slot) * F_Size);
         pragma Debug (Validate_Node_And_Offset_Write (N, Slot_Off));
      begin
         Set_Slot
           (N, Slot_Off,
            (S and not Shift_Left (Mask, V)) or Shift_Left (Slot (Val), V));
      end Set_1_Bit_Val;

      procedure Set_2_Bit_Val
        (N : Node_Or_Entity_Id; Offset : Field_Offset; Val : Field_Size_2_Bit)
      is
         F_Size : constant := 2;
         Mask : constant := 2**F_Size - 1;
         F_Per_Slot : constant Field_Offset := Slot_Size / F_Size;
         Slot_Off : constant Field_Offset := Offset / F_Per_Slot;
         S : constant Slot := Get_Slot (N, Slot_Off);
         V : constant Natural := Natural ((Offset mod F_Per_Slot) * F_Size);
         pragma Debug (Validate_Node_And_Offset_Write (N, Slot_Off));
      begin
         Set_Slot
           (N, Slot_Off,
            (S and not Shift_Left (Mask, V)) or Shift_Left (Slot (Val), V));
      end Set_2_Bit_Val;

      procedure Set_4_Bit_Val
        (N : Node_Or_Entity_Id; Offset : Field_Offset; Val : Field_Size_4_Bit)
      is
         F_Size : constant := 4;
         Mask : constant := 2**F_Size - 1;
         F_Per_Slot : constant Field_Offset := Slot_Size / F_Size;
         Slot_Off : constant Field_Offset := Offset / F_Per_Slot;
         S : constant Slot := Get_Slot (N, Slot_Off);
         V : constant Natural := Natural ((Offset mod F_Per_Slot) * F_Size);
         pragma Debug (Validate_Node_And_Offset_Write (N, Slot_Off));
      begin
         Set_Slot
           (N, Slot_Off,
            (S and not Shift_Left (Mask, V)) or Shift_Left (Slot (Val), V));
      end Set_4_Bit_Val;

      procedure Set_8_Bit_Val
        (N : Node_Or_Entity_Id; Offset : Field_Offset; Val : Field_Size_8_Bit)
      is
         F_Size : constant := 8;
         Mask : constant := 2**F_Size - 1;
         F_Per_Slot : constant Field_Offset := Slot_Size / F_Size;
         Slot_Off : constant Field_Offset := Offset / F_Per_Slot;
         S : constant Slot := Get_Slot (N, Slot_Off);
         V : constant Natural := Natural ((Offset mod F_Per_Slot) * F_Size);
         pragma Debug (Validate_Node_And_Offset_Write (N, Slot_Off));
      begin
         Set_Slot
           (N, Slot_Off,
            (S and not Shift_Left (Mask, V)) or Shift_Left (Slot (Val), V));
      end Set_8_Bit_Val;

      procedure Set_32_Bit_Val
        (N : Node_Or_Entity_Id; Offset : Field_Offset; Val : Field_Size_32_Bit)
      is
         F_Size : constant := 32;
         --  No Mask needed; this one doesn't do read-modify-write
         F_Per_Slot : constant Field_Offset := Slot_Size / F_Size;
         Slot_Off : constant Field_Offset := Offset / F_Per_Slot;
         pragma Debug (Validate_Node_And_Offset_Write (N, Slot_Off));
      begin
         Set_Slot (N, Slot_Off, Slot (Val));
      end Set_32_Bit_Val;

      ----------------------
      -- Print_Atree_Info --
      ----------------------

      procedure Print_Atree_Info (N : Node_Or_Entity_Id) is
         function Cast is new Ada.Unchecked_Conversion (Slot, Int);
      begin
         Write_Int (Int (Size_In_Slots (N)));
         Write_Str (" slots (");
         Write_Int (Int (Off_0 (N)));
         Write_Str (" .. ");
         Write_Int (Int (Off_L (N)));
         Write_Str ("):");

         for Off in Off_0 (N) .. Off_L (N) loop
            Write_Str (" ");
            Write_Int (Cast (Get_Slot (N, Off)));
         end loop;

         Write_Eol;
      end Print_Atree_Info;

   end Atree_Private_Part;

   ---------------------
   -- Get_Field_Value --
   ---------------------

   function Get_Node_Field_Union is new Get_32_Bit_Field (Union_Id)
     with Inline;
   --  Called when we don't know whether a field is a Node_Id or a List_Id,
   --  etc.

   function Get_Field_Value
     (N : Node_Id; Field : Node_Or_Entity_Field) return Field_Size_32_Bit
   is
      Desc : Field_Descriptor renames Field_Descriptors (Field);
      NN : constant Node_Or_Entity_Id := Node_To_Fetch_From (N, Field);

   begin
      case Field_Size (Desc.Kind) is
         when 1 => return Field_Size_32_Bit (Get_1_Bit_Val (NN, Desc.Offset));
         when 2 => return Field_Size_32_Bit (Get_2_Bit_Val (NN, Desc.Offset));
         when 4 => return Field_Size_32_Bit (Get_4_Bit_Val (NN, Desc.Offset));
         when 8 => return Field_Size_32_Bit (Get_8_Bit_Val (NN, Desc.Offset));
         when others => return Get_32_Bit_Val (NN, Desc.Offset);  -- 32
      end case;
   end Get_Field_Value;

   ---------------------
   -- Set_Field_Value --
   ---------------------

   procedure Set_Field_Value
     (N : Node_Id; Field : Node_Or_Entity_Field; Val : Field_Size_32_Bit)
   is
      Desc : Field_Descriptor renames Field_Descriptors (Field);

   begin
      case Field_Size (Desc.Kind) is
         when 1 => Set_1_Bit_Val (N, Desc.Offset, Field_Size_1_Bit (Val));
         when 2 => Set_2_Bit_Val (N, Desc.Offset, Field_Size_2_Bit (Val));
         when 4 => Set_4_Bit_Val (N, Desc.Offset, Field_Size_4_Bit (Val));
         when 8 => Set_8_Bit_Val (N, Desc.Offset, Field_Size_8_Bit (Val));
         when others => Set_32_Bit_Val (N, Desc.Offset, Val);  -- 32
      end case;
   end Set_Field_Value;

   procedure Reinit_Field_To_Zero
     (N : Node_Id; Field : Node_Or_Entity_Field)
   is
   begin
      Set_Field_Value (N, Field, 0);
   end Reinit_Field_To_Zero;

   function Field_Is_Initial_Zero
     (N : Node_Id; Field : Node_Or_Entity_Field) return Boolean is
   begin
      return Get_Field_Value (N, Field) = 0;
   end Field_Is_Initial_Zero;

   procedure Reinit_Field_To_Zero
     (N : Node_Id; Field : Entity_Field; Old_Ekind : Entity_Kind_Set) is
   begin
      pragma Assert (Old_Ekind (Ekind (N)), "Reinit: " & Ekind (N)'Img);
      Reinit_Field_To_Zero (N, Field);
   end Reinit_Field_To_Zero;

   procedure Reinit_Field_To_Zero
     (N : Node_Id; Field : Entity_Field; Old_Ekind : Entity_Kind) is
      Old_Ekind_Set : Entity_Kind_Set := (others => False);
   begin
      Old_Ekind_Set (Old_Ekind) := True;
      Reinit_Field_To_Zero (N, Field, Old_Ekind => Old_Ekind_Set);
   end Reinit_Field_To_Zero;

   procedure Check_Vanishing_Fields
     (Old_N : Node_Id; New_Kind : Node_Kind)
   is
      Old_Kind : constant Node_Kind := Nkind (Old_N);

      --  If this fails, it means you need to call Reinit_Field_To_Zero before
      --  calling Mutate_Nkind.

   begin
      for J in Node_Field_Table (Old_Kind)'Range loop
         declare
            F : constant Node_Field := Node_Field_Table (Old_Kind) (J);
         begin
            if not Field_Checking.Field_Present (New_Kind, F) then
               if not Field_Is_Initial_Zero (Old_N, F) then
                  Write_Str (Old_Kind'Img);
                  Write_Str (" --> ");
                  Write_Str (New_Kind'Img);
                  Write_Str (" Nonzero field ");
                  Write_Str (F'Img);
                  Write_Str (" is vanishing for node ");
                  Write_Int (Nat (Old_N));
                  Write_Eol;

                  raise Program_Error;
               end if;
            end if;
         end;
      end loop;
   end Check_Vanishing_Fields;

   procedure Check_Vanishing_Fields
     (Old_N : Entity_Id; New_Kind : Entity_Kind)
   is
      Old_Kind : constant Entity_Kind := Ekind (Old_N);

      --  If this fails, it means you need to call Reinit_Field_To_Zero before
      --  calling Mutate_Ekind. But we have many cases where vanishing fields
      --  are expected to reappear after converting to/from E_Void. Other cases
      --  are more problematic; set a breakpoint on "(non-E_Void case)" below.

   begin
      for J in Entity_Field_Table (Old_Kind)'Range loop
         declare
            F : constant Entity_Field := Entity_Field_Table (Old_Kind) (J);
         begin
            if not Field_Checking.Field_Present (New_Kind, F) then
               if not Field_Is_Initial_Zero (Old_N, F) then
                  Write_Str (Old_Kind'Img);
                  Write_Str (" --> ");
                  Write_Str (New_Kind'Img);
                  Write_Str (" Nonzero field ");
                  Write_Str (F'Img);
                  Write_Str (" is vanishing for node ");
                  Write_Int (Nat (Old_N));
                  Write_Eol;

                  if New_Kind = E_Void or else Old_Kind = E_Void then
                     Write_Line ("    (E_Void case)");
                  else
                     Write_Line ("    (non-E_Void case)");
                  end if;
               end if;
            end if;
         end;
      end loop;
   end Check_Vanishing_Fields;

   Nkind_Offset : constant Field_Offset :=
     Field_Descriptors (F_Nkind).Offset;

   procedure Set_Node_Kind_Type is new Set_8_Bit_Field (Node_Kind) with Inline;

   procedure Init_Nkind (N : Node_Id; Val : Node_Kind) is
      pragma Assert (Field_Is_Initial_Zero (N, F_Nkind));
   begin
      if Atree_Statistics_Enabled then
         Set_Count (F_Nkind) := Set_Count (F_Nkind) + 1;
      end if;

      Set_Node_Kind_Type (N, Nkind_Offset, Val);
   end Init_Nkind;

   procedure Mutate_Nkind
     (N : Node_Id; Val : Node_Kind; Old_Size : Slot_Count)
   is
      New_Size : constant Slot_Count := Size_In_Slots_To_Alloc (Val);

      All_Node_Offsets : Node_Offsets.Table_Type renames
        Node_Offsets.Table (Node_Offsets.First .. Node_Offsets.Last);
   begin
      pragma Debug (Check_Vanishing_Fields (N, Val));

      --  Grow the slots if necessary

      if Old_Size < New_Size then
         declare
            Old_Last_Slot : constant Node_Offset := Slots.Last;
            Old_Off_F : constant Node_Offset := Off_F (N);
         begin
            if Old_Last_Slot = Old_Off_F + Old_Size - 1 then
               --  In this case, the slots are at the end of Slots.Table, so we
               --  don't need to move them.
               Slots.Set_Last (Old_Last_Slot + New_Size - Old_Size);

            else
               --  Move the slots

               declare
                  New_Off_F : constant Node_Offset := Alloc_Slots (New_Size);
               begin
                  All_Node_Offsets (N).Offset := New_Off_F - N_Head;
                  Copy_Dynamic_Slots (Old_Off_F, New_Off_F, Old_Size);
                  pragma Debug
                    (Zero_Dynamic_Slots (Old_Off_F, Old_Off_F + Old_Size - 1));
               end;
            end if;
         end;

         Zero_Dynamic_Slots (Off_F (N) + Old_Size, Slots.Last);
      end if;

      if Atree_Statistics_Enabled then
         Set_Count (F_Nkind) := Set_Count (F_Nkind) + 1;
      end if;

      Set_Node_Kind_Type (N, Nkind_Offset, Val);
      pragma Debug (Validate_Node_Write (N));

      New_Node_Debugging_Output (N);
   end Mutate_Nkind;

   procedure Mutate_Nkind (N : Node_Id; Val : Node_Kind) is
   begin
      Mutate_Nkind (N, Val, Old_Size => Size_In_Slots_Dynamic (N));
   end Mutate_Nkind;

   Ekind_Offset : constant Field_Offset :=
     Field_Descriptors (F_Ekind).Offset;

   procedure Set_Entity_Kind_Type is new Set_8_Bit_Field (Entity_Kind)
     with Inline;

   procedure Mutate_Ekind
     (N : Entity_Id; Val : Entity_Kind)
   is
   begin
      if Ekind (N) = Val then
         return;
      end if;

      if Debug_Flag_Underscore_V then
         pragma Debug (Check_Vanishing_Fields (N, Val));
      end if;

      --  For now, we are allocating all entities with the same size, so we
      --  don't need to reallocate slots here.

      if Atree_Statistics_Enabled then
         Set_Count (F_Nkind) := Set_Count (F_Ekind) + 1;
      end if;

      Set_Entity_Kind_Type (N, Ekind_Offset, Val);
      pragma Debug (Validate_Node_Write (N));

      New_Node_Debugging_Output (N);
   end Mutate_Ekind;

   -----------------------
   -- Allocate_New_Node --
   -----------------------

   function Allocate_New_Node (Kind : Node_Kind) return Node_Id is
   begin
      return Result : constant Node_Id := Alloc_Node_Id do
         declare
            Sz : constant Slot_Count := Size_In_Slots_To_Alloc (Kind);
            Sl : constant Node_Offset := Alloc_Slots (Sz);
         begin
            Node_Offsets.Table (Result).Offset := Sl - N_Head;
            Zero_Dynamic_Slots (Sl, Sl + Sz - 1);
            Zero_Header_Slots (Result);
         end;

         Init_Nkind (Result, Kind);

         Orig_Nodes.Append (Result);
         Set_Comes_From_Source (Result, Comes_From_Source_Default);
         Allocate_List_Tables (Result);
         Report (Target => Result, Source => Empty);
      end return;
   end Allocate_New_Node;

   --------------------------
   -- Check_Error_Detected --
   --------------------------

   procedure Check_Error_Detected is
   begin
      --  An anomaly has been detected which is assumed to be a consequence of
      --  a previous serious error or configurable run time violation. Raise
      --  an exception if no such error has been detected.

      if Serious_Errors_Detected = 0
        and then Configurable_Run_Time_Violations = 0
      then
         raise Program_Error;
      end if;
   end Check_Error_Detected;

   -----------------
   -- Change_Node --
   -----------------

   procedure Change_Node (N : Node_Id; New_Kind : Node_Kind) is
      pragma Debug (Validate_Node_Write (N));
      pragma Assert (Nkind (N) not in N_Entity);
      pragma Assert (New_Kind not in N_Entity);

      Old_Size : constant Slot_Count := Size_In_Slots_Dynamic (N);
      New_Size : constant Slot_Count := Size_In_Slots_To_Alloc (New_Kind);

      Save_Sloc    : constant Source_Ptr := Sloc (N);
      Save_In_List : constant Boolean    := In_List (N);
      Save_CFS     : constant Boolean    := Comes_From_Source (N);
      Save_Posted  : constant Boolean    := Error_Posted (N);
      Save_CA      : constant Boolean    := Check_Actuals (N);
      Save_Is_IGN  : constant Boolean    := Is_Ignored_Ghost_Node (N);
      Save_Link    : constant Union_Id   := Link (N);

      Par_Count : Nat := 0;

   begin
      if Nkind (N) in N_Subexpr then
         Par_Count := Paren_Count (N);
      end if;

      if New_Size > Old_Size then
         declare
            New_Offset : constant Field_Offset := Alloc_Slots (New_Size);
         begin
            pragma Debug (Zero_Slots (N));
            Node_Offsets.Table (N).Offset := New_Offset - N_Head;
            Zero_Dynamic_Slots (New_Offset, New_Offset + New_Size - 1);
            Zero_Header_Slots (N);
         end;

      else
         Zero_Slots (N);
      end if;

      Init_Nkind (N, New_Kind); -- Not Mutate, because of Zero_Slots above

      Set_Sloc (N, Save_Sloc);
      Set_In_List (N, Save_In_List);
      Set_Comes_From_Source (N, Save_CFS);
      Set_Error_Posted (N, Save_Posted);
      Set_Check_Actuals (N, Save_CA);
      Set_Is_Ignored_Ghost_Node (N, Save_Is_IGN);
      Set_Link (N, Save_Link);

      if New_Kind in N_Subexpr then
         Set_Paren_Count (N, Par_Count);
      end if;
   end Change_Node;

   ----------------
   -- Copy_Slots --
   ----------------

   procedure Copy_Dynamic_Slots
     (From, To : Node_Offset; Num_Slots : Slot_Count)
   is
      pragma Assert (if Num_Slots /= 0 then From /= To);

      All_Slots : Slots.Table_Type renames
        Slots.Table (Slots.First .. Slots.Last);

      Source_Slots : Slots.Table_Type renames
        All_Slots (From .. From + Num_Slots - 1);

      Destination_Slots : Slots.Table_Type renames
        All_Slots (To .. To + Num_Slots - 1);

   begin
      Destination_Slots := Source_Slots;
   end Copy_Dynamic_Slots;

   procedure Copy_Slots (Source, Destination : Node_Id) is
      pragma Debug (Validate_Node (Source));
      pragma Assert (Source /= Destination);

      S_Size : constant Slot_Count := Size_In_Slots_Dynamic (Source);

      All_Node_Offsets : Node_Offsets.Table_Type renames
        Node_Offsets.Table (Node_Offsets.First .. Node_Offsets.Last);

   begin
      Copy_Dynamic_Slots
        (Off_F (Source), Off_F (Destination), S_Size);
      All_Node_Offsets (Destination).Slots := All_Node_Offsets (Source).Slots;
   end Copy_Slots;

   ---------------
   -- Copy_Node --
   ---------------

   procedure Copy_Node (Source, Destination : Node_Or_Entity_Id) is
      pragma Assert (Source /= Destination);

      Save_In_List : constant Boolean  := In_List (Destination);
      Save_Link    : constant Union_Id := Link (Destination);

      S_Size : constant Slot_Count := Size_In_Slots_To_Alloc (Source);
      D_Size : constant Slot_Count := Size_In_Slots_To_Alloc (Destination);

   begin
      New_Node_Debugging_Output (Source);
      New_Node_Debugging_Output (Destination);

      --  Currently all entities are allocated the same number of slots.
      --  Hopefully that won't always be the case, but if it is, the following
      --  is suboptimal if D_Size < S_Size, because in fact the Destination was
      --  allocated the max.

      --  If Source doesn't fit in Destination, we need to allocate

      if D_Size < S_Size then
         pragma Debug (Zero_Slots (Destination)); -- destroy old slots
         Node_Offsets.Table (Destination).Offset :=
           Alloc_Slots (S_Size) - N_Head;
      end if;

      Copy_Slots (Source, Destination);

      Set_In_List (Destination, Save_In_List);
      Set_Link (Destination, Save_Link);
      Set_Paren_Count_Of_Copy (Target => Destination, Source => Source);
   end Copy_Node;

   ------------------------
   -- Copy_Separate_List --
   ------------------------

   function Copy_Separate_List (Source : List_Id) return List_Id is
      Result : constant List_Id := New_List;
      Nod    : Node_Id := First (Source);

   begin
      while Present (Nod) loop
         Append (Copy_Separate_Tree (Nod), Result);
         Next (Nod);
      end loop;

      return Result;
   end Copy_Separate_List;

   ------------------------
   -- Copy_Separate_Tree --
   ------------------------

   function Copy_Separate_Tree (Source : Node_Id) return Node_Id is

      pragma Debug (Validate_Node (Source));

      New_Id : Node_Id;

      function Copy_Entity (E : Entity_Id) return Entity_Id;
      --  Copy Entity, copying only Chars field

      function Copy_List (List : List_Id) return List_Id;
      --  Copy list

      function Possible_Copy (Field : Union_Id) return Union_Id;
      --  Given a field, returns a copy of the node or list if its parent is
      --  the current source node, and otherwise returns the input.

      -----------------
      -- Copy_Entity --
      -----------------

      function Copy_Entity (E : Entity_Id) return Entity_Id is
      begin
         pragma Assert (Nkind (E) in N_Entity);

         return Result : constant Entity_Id := New_Entity (Nkind (E), Sloc (E))
         do
            Set_Chars (Result, Chars (E));
         end return;
      end Copy_Entity;

      ---------------
      -- Copy_List --
      ---------------

      function Copy_List (List : List_Id) return List_Id is
         NL : List_Id;
         E  : Node_Id;

      begin
         if List = No_List then
            return No_List;

         else
            NL := New_List;

            E := First (List);
            while Present (E) loop
               if Is_Entity (E) then
                  Append (Copy_Entity (E), NL);
               else
                  Append (Copy_Separate_Tree (E), NL);
               end if;

               Next (E);
            end loop;

            return NL;
         end if;
      end Copy_List;

      -------------------
      -- Possible_Copy --
      -------------------

      function Possible_Copy (Field : Union_Id) return Union_Id is
         New_N : Union_Id;

      begin
         if Field in Node_Range then
            New_N := Union_Id (Copy_Separate_Tree (Node_Id (Field)));

            if Present (Node_Id (Field))
              and then Parent (Node_Id (Field)) = Source
            then
               Set_Parent (Node_Id (New_N), New_Id);
            end if;

            return New_N;

         elsif Field in List_Range then
            New_N := Union_Id (Copy_List (List_Id (Field)));

            if Parent (List_Id (Field)) = Source then
               Set_Parent (List_Id (New_N), New_Id);
            end if;

            return New_N;

         else
            return Field;
         end if;
      end Possible_Copy;

      procedure Walk is new Walk_Sinfo_Fields_Pairwise (Possible_Copy);

   --  Start of processing for Copy_Separate_Tree

   begin
      if Source <= Empty_Or_Error then
         return Source;

      elsif Is_Entity (Source) then
         return Copy_Entity (Source);

      else
         New_Id := New_Copy (Source);

         Walk (New_Id, Source);

         --  Explicitly copy the aspect specifications as those do not reside
         --  in a node field.

         if Permits_Aspect_Specifications (Source)
           and then Has_Aspects (Source)
         then
            Set_Aspect_Specifications
              (New_Id, Copy_List (Aspect_Specifications (Source)));
         end if;

         --  Set Entity field to Empty to ensure that no entity references
         --  are shared between the two, if the source is already analyzed.

         if Nkind (New_Id) in N_Has_Entity
           or else Nkind (New_Id) = N_Freeze_Entity
         then
            Set_Entity (New_Id, Empty);
         end if;

         --  Reset all Etype fields and Analyzed flags, because input tree may
         --  have been fully or partially analyzed.

         if Nkind (New_Id) in N_Has_Etype then
            Set_Etype (New_Id, Empty);
         end if;

         Set_Analyzed (New_Id, False);

         --  Rather special case, if we have an expanded name, then change
         --  it back into a selected component, so that the tree looks the
         --  way it did coming out of the parser. This will change back
         --  when we analyze the selected component node.

         if Nkind (New_Id) = N_Expanded_Name then

            --  The following code is a bit kludgy. It would be cleaner to
            --  Add an entry Change_Expanded_Name_To_Selected_Component to
            --  Sinfo.CN, but that's delicate because Atree is used in the
            --  binder, so we don't want to add that dependency.
            --  ??? Revisit now that ASIS is no longer using this unit.

            --  Consequently we have no choice but to hold our noses and do the
            --  change manually. At least we are Atree, so this is at least all
            --  in the family.

            --  Clear the Chars field which is not present in a selected
            --  component node, so we don't want a junk value around. Note that
            --  we can't just call Set_Chars, because Empty is of the wrong
            --  type, and is outside the range of Name_Id.

            Reinit_Field_To_Zero (New_Id, F_Chars);
            Reinit_Field_To_Zero (New_Id, F_Has_Private_View);
            Reinit_Field_To_Zero (New_Id, F_Is_Elaboration_Checks_OK_Node);
            Reinit_Field_To_Zero (New_Id, F_Is_Elaboration_Warnings_OK_Node);
            Reinit_Field_To_Zero (New_Id, F_Is_SPARK_Mode_On_Node);

            --  Change the node type

            Mutate_Nkind (New_Id, N_Selected_Component);
         end if;

         --  All done, return copied node

         return New_Id;
      end if;
   end Copy_Separate_Tree;

   -----------------------
   -- Exchange_Entities --
   -----------------------

   procedure Exchange_Entities (E1 : Entity_Id; E2 : Entity_Id) is
      pragma Debug (Validate_Node_Write (E1));
      pragma Debug (Validate_Node_Write (E2));
      pragma Assert
        (Is_Entity (E1) and then Is_Entity (E2)
           and then not In_List (E1) and then not In_List (E2));

      Old_E1 : constant Node_Header := Node_Offsets.Table (E1);

   begin
      Node_Offsets.Table (E1) := Node_Offsets.Table (E2);
      Node_Offsets.Table (E2) := Old_E1;

      --  That exchange exchanged the parent pointers as well, which is what
      --  we want, but we need to patch up the defining identifier pointers
      --  in the parent nodes (the child pointers) to match this switch
      --  unless for Implicit types entities which have no parent, in which
      --  case we don't do anything otherwise we won't be able to revert back
      --  to the original situation.

      --  Shouldn't this use Is_Itype instead of the Parent test???

      if Present (Parent (E1)) and then Present (Parent (E2)) then
         Set_Defining_Identifier (Parent (E1), E1);
         Set_Defining_Identifier (Parent (E2), E2);
      end if;

      New_Node_Debugging_Output (E1);
      New_Node_Debugging_Output (E2);
   end Exchange_Entities;

   -----------------
   -- Extend_Node --
   -----------------

   procedure Extend_Node (Source : Node_Id) is
      pragma Assert (Present (Source));
      pragma Assert (not Is_Entity (Source));

      Old_Kind : constant Node_Kind := Nkind (Source);
      pragma Assert (Old_Kind in N_Direct_Name);
      New_Kind : constant Node_Kind :=
        (case Old_Kind is
           when N_Character_Literal => N_Defining_Character_Literal,
           when N_Identifier => N_Defining_Identifier,
           when N_Operator_Symbol => N_Defining_Operator_Symbol,
           when others => N_Unused_At_Start); -- can't happen
      --  The new NKind, which is the appropriate value of N_Entity based on
      --  the old Nkind. N_xxx is mapped to N_Defining_xxx.
      pragma Assert (New_Kind in N_Entity);

   --  Start of processing for Extend_Node

   begin
      Set_Check_Actuals (Source, False);
      Mutate_Nkind (Source, New_Kind);
      Report (Target => Source, Source => Source);
   end Extend_Node;

   -----------------
   -- Fix_Parents --
   -----------------

   procedure Fix_Parents (Ref_Node, Fix_Node : Node_Id) is
      pragma Assert (Nkind (Ref_Node) = Nkind (Fix_Node));

      procedure Fix_Parent (Field : Union_Id);
      --  Fix up one parent pointer. Field is checked to see if it points to
      --  a node, list, or element list that has a parent that points to
      --  Ref_Node. If so, the parent is reset to point to Fix_Node.

      ----------------
      -- Fix_Parent --
      ----------------

      procedure Fix_Parent (Field : Union_Id) is
      begin
         --  Fix parent of node that is referenced by Field. Note that we must
         --  exclude the case where the node is a member of a list, because in
         --  this case the parent is the parent of the list.

         if Field in Node_Range
           and then Present (Node_Id (Field))
           and then not In_List (Node_Id (Field))
           and then Parent (Node_Id (Field)) = Ref_Node
         then
            Set_Parent (Node_Id (Field), Fix_Node);

         --  Fix parent of list that is referenced by Field

         elsif Field in List_Range
           and then Present (List_Id (Field))
           and then Parent (List_Id (Field)) = Ref_Node
         then
            Set_Parent (List_Id (Field), Fix_Node);
         end if;
      end Fix_Parent;

      Fields : Node_Field_Array renames
        Node_Field_Table (Nkind (Fix_Node)).all;

   --  Start of processing for Fix_Parents

   begin
      for J in Fields'Range loop
         declare
            Desc : Field_Descriptor renames Field_Descriptors (Fields (J));
         begin
            if Desc.Kind in Node_Id_Field | List_Id_Field then
               Fix_Parent (Get_Node_Field_Union (Fix_Node, Desc.Offset));
            end if;
         end;
      end loop;
   end Fix_Parents;

   -----------------------------------
   -- Get_Comes_From_Source_Default --
   -----------------------------------

   function Get_Comes_From_Source_Default return Boolean is
   begin
      return Comes_From_Source_Default;
   end Get_Comes_From_Source_Default;

   ---------------
   -- Is_Entity --
   ---------------

   function Is_Entity (N : Node_Or_Entity_Id) return Boolean is
   begin
      return Nkind (N) in N_Entity;
   end Is_Entity;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Dummy : Node_Id;
      pragma Warnings (Off, Dummy);

   begin
      --  Allocate Empty node

      Dummy := New_Node (N_Empty, No_Location);
      Set_Chars (Empty, No_Name);
      pragma Assert (Dummy = Empty);

      --  Allocate Error node, and set Error_Posted, since we certainly
      --  only generate an Error node if we do post some kind of error.

      Dummy := New_Node (N_Error, No_Location);
      Set_Chars (Error, Error_Name);
      Set_Error_Posted (Error, True);
      pragma Assert (Dummy = Error);
   end Initialize;

   --------------------------
   -- Is_Rewrite_Insertion --
   --------------------------

   function Is_Rewrite_Insertion (Node : Node_Id) return Boolean is
   begin
      return Rewrite_Ins (Node);
   end Is_Rewrite_Insertion;

   -----------------------------
   -- Is_Rewrite_Substitution --
   -----------------------------

   function Is_Rewrite_Substitution (Node : Node_Id) return Boolean is
   begin
      return Orig_Nodes.Table (Node) /= Node;
   end Is_Rewrite_Substitution;

   ------------------
   -- Last_Node_Id --
   ------------------

   function Last_Node_Id return Node_Id is
   begin
      return Node_Offsets.Last;
   end Last_Node_Id;

   ----------
   -- Lock --
   ----------

   procedure Lock is
   begin
      Orig_Nodes.Locked := True;
   end Lock;

   ----------------
   -- Lock_Nodes --
   ----------------

   procedure Lock_Nodes is
   begin
      pragma Assert (not Locked);
      Locked := True;
   end Lock_Nodes;

   -------------------------
   -- Mark_New_Ghost_Node --
   -------------------------

   procedure Mark_New_Ghost_Node (N : Node_Or_Entity_Id) is
   begin
      pragma Debug (Validate_Node_Write (N));

      --  The Ghost node is created within a Ghost region

      if Ghost_Mode = Check then
         if Nkind (N) in N_Entity then
            Set_Is_Checked_Ghost_Entity (N);
         end if;

      elsif Ghost_Mode = Ignore then
         if Nkind (N) in N_Entity then
            Set_Is_Ignored_Ghost_Entity (N);
         end if;

         Set_Is_Ignored_Ghost_Node (N);

         --  Record the ignored Ghost node or entity in order to eliminate it
         --  from the tree later.

         if Ignored_Ghost_Recording_Proc /= null then
            Ignored_Ghost_Recording_Proc.all (N);
         end if;
      end if;
   end Mark_New_Ghost_Node;

   ----------------------------
   -- Mark_Rewrite_Insertion --
   ----------------------------

   procedure Mark_Rewrite_Insertion (New_Node : Node_Id) is
   begin
      Set_Rewrite_Ins (New_Node);
   end Mark_Rewrite_Insertion;

   --------------
   -- New_Copy --
   --------------

   function New_Copy (Source : Node_Id) return Node_Id is
      pragma Debug (Validate_Node (Source));
      S_Size : constant Slot_Count := Size_In_Slots_To_Alloc (Source);
   begin
      if Source <= Empty_Or_Error then
         return Source;
      end if;

      return New_Id : constant Node_Id := Alloc_Node_Id do
         Node_Offsets.Table (New_Id).Offset :=
           Alloc_Slots (S_Size) - N_Head;
         Orig_Nodes.Append (New_Id);
         Copy_Slots (Source, New_Id);

         Set_Check_Actuals (New_Id, False);
         Set_Paren_Count_Of_Copy (Target => New_Id, Source => Source);

         Allocate_List_Tables (New_Id);
         Report (Target => New_Id, Source => Source);

         Set_In_List (New_Id, False);
         Set_Link (New_Id, Empty_List_Or_Node);

         --  If the original is marked as a rewrite insertion, then unmark the
         --  copy, since we inserted the original, not the copy.

         Set_Rewrite_Ins (New_Id, False);

         --  Clear Is_Overloaded since we cannot have semantic interpretations
         --  of this new node.

         if Nkind (Source) in N_Subexpr then
            Set_Is_Overloaded (New_Id, False);
         end if;

         --  Always clear Has_Aspects, the caller must take care of copying
         --  aspects if this is required for the particular situation.

         Set_Has_Aspects (New_Id, False);

         --  Mark the copy as Ghost depending on the current Ghost region

         if Nkind (New_Id) in N_Entity then
            Set_Is_Checked_Ghost_Entity (New_Id, False);
            Set_Is_Ignored_Ghost_Entity (New_Id, False);
         end if;

         Mark_New_Ghost_Node (New_Id);

         New_Node_Debugging_Output (New_Id);

         pragma Assert (New_Id /= Source);
      end return;
   end New_Copy;

   ----------------
   -- New_Entity --
   ----------------

   function New_Entity
     (New_Node_Kind : Node_Kind;
      New_Sloc      : Source_Ptr) return Entity_Id
   is
      pragma Assert (New_Node_Kind in N_Entity);
      New_Id : constant Entity_Id := Allocate_New_Node (New_Node_Kind);
      pragma Assert (Original_Node (Node_Offsets.Last) = Node_Offsets.Last);
   begin
      --  If this is a node with a real location and we are generating
      --  source nodes, then reset Current_Error_Node. This is useful
      --  if we bomb during parsing to get a error location for the bomb.

      if New_Sloc > No_Location and then Comes_From_Source_Default then
         Current_Error_Node := New_Id;
      end if;

      Set_Sloc (New_Id, New_Sloc);

      --  Mark the new entity as Ghost depending on the current Ghost region

      Mark_New_Ghost_Node (New_Id);

      New_Node_Debugging_Output (New_Id);

      return New_Id;
   end New_Entity;

   --------------
   -- New_Node --
   --------------

   function New_Node
     (New_Node_Kind : Node_Kind;
      New_Sloc      : Source_Ptr) return Node_Id
   is
      pragma Assert (New_Node_Kind not in N_Entity);
      New_Id : constant Node_Id := Allocate_New_Node (New_Node_Kind);
      pragma Assert (Original_Node (Node_Offsets.Last) = Node_Offsets.Last);
   begin
      Set_Sloc (New_Id, New_Sloc);

      --  If this is a node with a real location and we are generating source
      --  nodes, then reset Current_Error_Node. This is useful if we bomb
      --  during parsing to get an error location for the bomb.

      if Comes_From_Source_Default and then New_Sloc > No_Location then
         Current_Error_Node := New_Id;
      end if;

      --  Mark the new node as Ghost depending on the current Ghost region

      Mark_New_Ghost_Node (New_Id);

      New_Node_Debugging_Output (New_Id);

      return New_Id;
   end New_Node;

   --------
   -- No --
   --------

   function No (N : Node_Id) return Boolean is
   begin
      return N = Empty;
   end No;

   -------------------
   -- Nodes_Address --
   -------------------

   function Node_Offsets_Address return System.Address is
   begin
      return Node_Offsets.Table (First_Node_Id)'Address;
   end Node_Offsets_Address;

   function Slots_Address return System.Address is
      Slot_Byte_Size : constant := 4;
      pragma Assert (Slot_Byte_Size * 8 = Slot'Size);
      Extra : constant := Slots_Low_Bound * Slot_Byte_Size;
      --  Slots does not start at 0, so we need to subtract off the extra
      --  amount. We are returning Slots.Table (0)'Address, except that
      --  that component does not exist.
      use System.Storage_Elements;
   begin
      return Slots.Table (Slots_Low_Bound)'Address - Extra;
   end Slots_Address;

   -----------------------------------
   -- Approx_Num_Nodes_And_Entities --
   -----------------------------------

   function Approx_Num_Nodes_And_Entities return Nat is
   begin
      return Nat (Node_Offsets.Last - First_Node_Id);
   end Approx_Num_Nodes_And_Entities;

   -----------
   -- Off_0 --
   -----------

   function Off_0 (N : Node_Id) return Node_Offset'Base is
      pragma Debug (Validate_Node (N));

      All_Node_Offsets : Node_Offsets.Table_Type renames
        Node_Offsets.Table (Node_Offsets.First .. Node_Offsets.Last);
   begin
      return All_Node_Offsets (N).Offset;
   end Off_0;

   -----------
   -- Off_F --
   -----------

   function Off_F (N : Node_Id) return Node_Offset is
   begin
      return Off_0 (N) + N_Head;
   end Off_F;

   -----------
   -- Off_L --
   -----------

   function Off_L (N : Node_Id) return Node_Offset is
      pragma Debug (Validate_Node (N));

      All_Node_Offsets : Node_Offsets.Table_Type renames
        Node_Offsets.Table (Node_Offsets.First .. Node_Offsets.Last);
   begin
      return All_Node_Offsets (N).Offset + Size_In_Slots (N) - 1;
   end Off_L;

   -------------------
   -- Original_Node --
   -------------------

   function Original_Node (Node : Node_Id) return Node_Id is
   begin
      pragma Debug (Validate_Node (Node));
      if Atree_Statistics_Enabled then
         Get_Original_Node_Count := Get_Original_Node_Count + 1;
      end if;

      return Orig_Nodes.Table (Node);
   end Original_Node;

   -----------------
   -- Paren_Count --
   -----------------

   function Paren_Count (N : Node_Id) return Nat is
      pragma Debug (Validate_Node (N));

      C : constant Small_Paren_Count_Type := Small_Paren_Count (N);

   begin
      --  Value of 0,1,2 returned as is

      if C <= 2 then
         return C;

      --  Value of 3 means we search the table, and we must find an entry

      else
         for J in Paren_Counts.First .. Paren_Counts.Last loop
            if N = Paren_Counts.Table (J).Nod then
               return Paren_Counts.Table (J).Count;
            end if;
         end loop;

         raise Program_Error;
      end if;
   end Paren_Count;

   function Node_Parent (N : Node_Or_Entity_Id) return Node_Or_Entity_Id is
   begin
      pragma Assert (Present (N));

      if Is_List_Member (N) then
         return Parent (List_Containing (N));
      else
         return Node_Or_Entity_Id (Link (N));
      end if;
   end Node_Parent;

   -------------
   -- Present --
   -------------

   function Present (N : Node_Id) return Boolean is
   begin
      return N /= Empty;
   end Present;

   --------------------------------
   -- Preserve_Comes_From_Source --
   --------------------------------

   procedure Preserve_Comes_From_Source (NewN, OldN : Node_Id) is
   begin
      Set_Comes_From_Source (NewN, Comes_From_Source (OldN));
   end Preserve_Comes_From_Source;

   -------------------
   -- Relocate_Node --
   -------------------

   function Relocate_Node (Source : Node_Id) return Node_Id is
      New_Node : Node_Id;

   begin
      if No (Source) then
         return Empty;
      end if;

      New_Node := New_Copy (Source);
      Fix_Parents (Ref_Node => Source, Fix_Node => New_Node);

      --  We now set the parent of the new node to be the same as the parent of
      --  the source. Almost always this parent will be replaced by a new value
      --  when the relocated node is reattached to the tree, but by doing it
      --  now, we ensure that this node is not even temporarily disconnected
      --  from the tree. Note that this does not happen free, because in the
      --  list case, the parent does not get set.

      Set_Parent (New_Node, Parent (Source));

      --  If the node being relocated was a rewriting of some original node,
      --  then the relocated node has the same original node.

      if Is_Rewrite_Substitution (Source) then
         Set_Original_Node (New_Node, Original_Node (Source));
      end if;

      --  If we're relocating a subprogram call and we're doing
      --  unnesting, be sure we make a new copy of any parameter associations
      --  so that we don't share them.

      if Nkind (Source) in N_Subprogram_Call
        and then Opt.Unnest_Subprogram_Mode
        and then Present (Parameter_Associations (Source))
      then
         declare
            New_Assoc : constant List_Id := Parameter_Associations (Source);
         begin
            Set_Parent (New_Assoc, New_Node);
            Set_Parameter_Associations (New_Node, New_Assoc);
         end;
      end if;

      return New_Node;
   end Relocate_Node;

   -------------
   -- Replace --
   -------------

   procedure Replace (Old_Node, New_Node : Node_Id) is
      Old_Post : constant Boolean := Error_Posted (Old_Node);
      Old_HasA : constant Boolean := Has_Aspects (Old_Node);
      Old_CFS  : constant Boolean := Comes_From_Source (Old_Node);

      procedure Destroy_New_Node;
      --  Overwrite New_Node data with junk, for debugging purposes

      procedure Destroy_New_Node is
      begin
         Zero_Slots (New_Node);
         Node_Offsets.Table (New_Node).Offset := Field_Offset'Base'Last;
      end Destroy_New_Node;

   begin
      New_Node_Debugging_Output (Old_Node);
      New_Node_Debugging_Output (New_Node);

      pragma Assert
        (not Is_Entity (Old_Node)
          and not Is_Entity (New_Node)
          and not In_List (New_Node)
          and Old_Node /= New_Node);

      --  Do copy, preserving link and in list status and required flags

      Copy_Node (Source => New_Node, Destination => Old_Node);
      Set_Comes_From_Source (Old_Node, Old_CFS);
      Set_Error_Posted      (Old_Node, Old_Post);
      Set_Has_Aspects       (Old_Node, Old_HasA);

      --  Fix parents of substituted node, since it has changed identity

      Fix_Parents (Ref_Node => New_Node, Fix_Node => Old_Node);

      pragma Debug (Destroy_New_Node);

      --  Since we are doing a replace, we assume that the original node
      --  is intended to become the new replaced node. The call would be
      --  to Rewrite if there were an intention to save the original node.

      Set_Original_Node (Old_Node, Old_Node);

      --  Invoke the reporting procedure (if available)

      if Reporting_Proc /= null then
         Reporting_Proc.all (Target => Old_Node, Source => New_Node);
      end if;
   end Replace;

   ------------
   -- Report --
   ------------

   procedure Report (Target, Source : Node_Id) is
   begin
      if Reporting_Proc /= null then
         Reporting_Proc.all (Target, Source);
      end if;
   end Report;

   -------------
   -- Rewrite --
   -------------

   procedure Rewrite (Old_Node, New_Node : Node_Id) is
      Old_CA     : constant Boolean := Check_Actuals (Old_Node);
      Old_Is_IGN : constant Boolean := Is_Ignored_Ghost_Node (Old_Node);
      Old_Error_Posted : constant Boolean :=
                           Error_Posted (Old_Node);
      Old_Has_Aspects  : constant Boolean :=
                           Has_Aspects (Old_Node);

      Old_Must_Not_Freeze : constant Boolean :=
        (if Nkind (Old_Node) in N_Subexpr then Must_Not_Freeze (Old_Node)
         else False);
      Old_Paren_Count     : constant Nat :=
        (if Nkind (Old_Node) in N_Subexpr then Paren_Count (Old_Node) else 0);
      --  These fields are preserved in the new node only if the new node and
      --  the old node are both subexpression nodes. We might be changing Nkind
      --  (Old_Node) from not N_Subexpr to N_Subexpr, so we need a value
      --  (False/0) even if Old_Noed is not a N_Subexpr.

      --  Note: it is a violation of abstraction levels for Must_Not_Freeze
      --  to be referenced like this. ???

      Sav_Node : Node_Id;

   begin
      New_Node_Debugging_Output (Old_Node);
      New_Node_Debugging_Output (New_Node);

      pragma Assert
        (not Is_Entity (Old_Node)
          and not Is_Entity (New_Node)
          and not In_List (New_Node));

      --  Allocate a new node, to be used to preserve the original contents
      --  of the Old_Node, for possible later retrival by Original_Node and
      --  make an entry in the Orig_Nodes table. This is only done if we have
      --  not already rewritten the node, as indicated by an Orig_Nodes entry
      --  that does not reference the Old_Node.

      if not Is_Rewrite_Substitution (Old_Node) then
         Sav_Node := New_Copy (Old_Node);
         Set_Original_Node (Sav_Node, Sav_Node);
         Set_Original_Node (Old_Node, Sav_Node);

         --  Both the old and new copies of the node will share the same list
         --  of aspect specifications if aspect specifications are present.
         --  Restore the parent link of the aspect list to the old node, which
         --  is the one linked in the tree.

         if Old_Has_Aspects then
            declare
               Aspects : constant List_Id := Aspect_Specifications (Old_Node);
            begin
               Set_Aspect_Specifications (Sav_Node, Aspects);
               Set_Parent (Aspects, Old_Node);
            end;
         end if;
      end if;

      --  Copy substitute node into place, preserving old fields as required

      Copy_Node (Source => New_Node, Destination => Old_Node);
      Set_Error_Posted (Old_Node, Old_Error_Posted);
      Set_Has_Aspects  (Old_Node, Old_Has_Aspects);

      Set_Check_Actuals (Old_Node, Old_CA);
      Set_Is_Ignored_Ghost_Node (Old_Node, Old_Is_IGN);

      if Nkind (New_Node) in N_Subexpr then
         Set_Paren_Count     (Old_Node, Old_Paren_Count);
         Set_Must_Not_Freeze (Old_Node, Old_Must_Not_Freeze);
      end if;

      Fix_Parents (Ref_Node => New_Node, Fix_Node => Old_Node);

      --  Invoke the reporting procedure (if available)

      if Reporting_Proc /= null then
         Reporting_Proc.all (Target => Old_Node, Source => New_Node);
      end if;

      --  Invoke the rewriting procedure (if available)

      if Rewriting_Proc /= null then
         Rewriting_Proc.all (Target => Old_Node, Source => New_Node);
      end if;
   end Rewrite;

   -----------------------------------
   -- Set_Comes_From_Source_Default --
   -----------------------------------

   procedure Set_Comes_From_Source_Default (Default : Boolean) is
   begin
      Comes_From_Source_Default := Default;
   end Set_Comes_From_Source_Default;

   --------------------------------------
   -- Set_Ignored_Ghost_Recording_Proc --
   --------------------------------------

   procedure Set_Ignored_Ghost_Recording_Proc
     (Proc : Ignored_Ghost_Record_Proc)
   is
   begin
      pragma Assert (Ignored_Ghost_Recording_Proc = null);
      Ignored_Ghost_Recording_Proc := Proc;
   end Set_Ignored_Ghost_Recording_Proc;

   -----------------------
   -- Set_Original_Node --
   -----------------------

   procedure Set_Original_Node (N : Node_Id; Val : Node_Id) is
   begin
      pragma Debug (Validate_Node_Write (N));
      if Atree_Statistics_Enabled then
         Set_Original_Node_Count := Set_Original_Node_Count + 1;
      end if;

      Orig_Nodes.Table (N) := Val;
   end Set_Original_Node;

   ---------------------
   -- Set_Paren_Count --
   ---------------------

   procedure Set_Paren_Count (N : Node_Id; Val : Nat) is
   begin
      pragma Debug (Validate_Node_Write (N));
      pragma Assert (Nkind (N) in N_Subexpr);

      --  Value of 0,1,2 stored as is

      if Val <= 2 then
         Set_Small_Paren_Count (N, Val);

      --  Value of 3 or greater stores 3 in node and makes table entry

      else
         Set_Small_Paren_Count (N, 3);

         --  Search for existing table entry

         for J in Paren_Counts.First .. Paren_Counts.Last loop
            if N = Paren_Counts.Table (J).Nod then
               Paren_Counts.Table (J).Count := Val;
               return;
            end if;
         end loop;

         --  No existing table entry; make a new one

         Paren_Counts.Append ((Nod => N, Count => Val));
      end if;
   end Set_Paren_Count;

   -----------------------------
   -- Set_Paren_Count_Of_Copy --
   -----------------------------

   procedure Set_Paren_Count_Of_Copy (Target, Source : Node_Id) is
   begin
      --  We already copied the Small_Paren_Count. We need to update the
      --  Paren_Counts table only if greater than 2.

      if Nkind (Source) in N_Subexpr
        and then Small_Paren_Count (Source) = 3
      then
         Set_Paren_Count (Target, Paren_Count (Source));
      end if;

      pragma Assert (Paren_Count (Target) = Paren_Count (Source));
   end Set_Paren_Count_Of_Copy;

   ----------------
   -- Set_Parent --
   ----------------

   procedure Set_Node_Parent (N : Node_Or_Entity_Id; Val : Node_Or_Entity_Id) is
   begin
      pragma Assert (Present (N));
      pragma Assert (not In_List (N));
      Set_Link (N, Union_Id (Val));
   end Set_Node_Parent;

   ------------------------
   -- Set_Reporting_Proc --
   ------------------------

   procedure Set_Reporting_Proc (Proc : Report_Proc) is
   begin
      pragma Assert (Reporting_Proc = null);
      Reporting_Proc := Proc;
   end Set_Reporting_Proc;

   ------------------------
   -- Set_Rewriting_Proc --
   ------------------------

   procedure Set_Rewriting_Proc (Proc : Rewrite_Proc) is
   begin
      pragma Assert (Rewriting_Proc = null);
      Rewriting_Proc := Proc;
   end Set_Rewriting_Proc;

   ----------------------------
   -- Size_In_Slots_To_Alloc --
   ----------------------------

   function Size_In_Slots_To_Alloc (Kind : Node_Kind) return Slot_Count is
   begin
      return
        (if Kind in N_Entity then Einfo.Entities.Max_Entity_Size
         else Sinfo.Nodes.Size (Kind)) - N_Head;
      --  Unfortunately, we don't know the Entity_Kind, so we have to use the
      --  max.
   end Size_In_Slots_To_Alloc;

   function Size_In_Slots_To_Alloc
     (N : Node_Or_Entity_Id) return Slot_Count is
   begin
      return Size_In_Slots_To_Alloc (Nkind (N));
   end Size_In_Slots_To_Alloc;

   -------------------
   -- Size_In_Slots --
   -------------------

   function Size_In_Slots (N : Node_Or_Entity_Id) return Slot_Count is
   begin
      pragma Assert (Nkind (N) /= N_Unused_At_Start);
      return
        (if Nkind (N) in N_Entity then Einfo.Entities.Max_Entity_Size
         else Sinfo.Nodes.Size (Nkind (N)));
   end Size_In_Slots;

   ---------------------------
   -- Size_In_Slots_Dynamic --
   ---------------------------

   function Size_In_Slots_Dynamic (N : Node_Or_Entity_Id) return Slot_Count is
   begin
      return Size_In_Slots (N) - N_Head;
   end Size_In_Slots_Dynamic;

   -----------------------------------
   -- Internal_Traverse_With_Parent --
   -----------------------------------

   function Internal_Traverse_With_Parent
      (Node : Node_Id) return Traverse_Final_Result
   is
      Tail_Recursion_Counter : Natural := 0;

      procedure Pop_Parents;
      --  Pop enclosing nodes of tail recursion plus the current parent.

      function Traverse_Field (Fld : Union_Id) return Traverse_Final_Result;
      --  Fld is one of the Traversed fields of Nod, which is necessarily a
      --  Node_Id or List_Id. It is traversed, and the result is the result of
      --  this traversal.

      -----------------
      -- Pop_Parents --
      -----------------

      procedure Pop_Parents is
      begin
         --  Pop the enclosing nodes of the tail recursion

         for J in 1 .. Tail_Recursion_Counter loop
            Parents_Stack.Decrement_Last;
         end loop;

         --  Pop the current node

         pragma Assert (Parents_Stack.Table (Parents_Stack.Last) = Node);
         Parents_Stack.Decrement_Last;
      end Pop_Parents;

      --------------------
      -- Traverse_Field --
      --------------------

      function Traverse_Field (Fld : Union_Id) return Traverse_Final_Result is
      begin
         if Fld /= Union_Id (Empty) then

            --  Descendant is a node

            if Fld in Node_Range then
               return Internal_Traverse_With_Parent (Node_Id (Fld));

            --  Descendant is a list

            elsif Fld in List_Range then
               declare
                  Elmt : Node_Id := First (List_Id (Fld));
               begin
                  while Present (Elmt) loop
                     if Internal_Traverse_With_Parent (Elmt) = Abandon then
                        return Abandon;
                     end if;

                     Next (Elmt);
                  end loop;
               end;

            else
               raise Program_Error;
            end if;
         end if;

         return OK;
      end Traverse_Field;

      --  Local variables

      Parent_Node : Node_Id := Parents_Stack.Table (Parents_Stack.Last);
      Cur_Node    : Node_Id := Node;

   --  Start of processing for Internal_Traverse_With_Parent

   begin
      --  If the last field is a node, we eliminate the tail recursion by
      --  jumping back to this label. This is because concatenations are
      --  sometimes deeply nested, as in X1&X2&...&Xn. Gen_IL ensures that the
      --  Left_Opnd field of N_Op_Concat comes last in Traversed_Fields, so the
      --  tail recursion is eliminated in that case. This trick prevents us
      --  from running out of stack memory in that case. We don't bother
      --  eliminating the tail recursion if the last field is a list.

      <<Tail_Recurse>>

      Parents_Stack.Append (Cur_Node);

      case Process (Parent_Node, Cur_Node) is
         when Abandon =>
            Pop_Parents;
            return Abandon;

         when Skip =>
            Pop_Parents;
            return OK;

         when OK =>
            null;

         when OK_Orig =>
            Cur_Node := Original_Node (Cur_Node);
      end case;

      --  Check for empty Traversed_Fields before entering loop below, so the
      --  tail recursive step won't go past the end.

      declare
         Cur_Field : Offset_Array_Index := Traversed_Offset_Array'First;
         Offsets : Traversed_Offset_Array renames
           Traversed_Fields (Nkind (Cur_Node));

      begin
         if Offsets (Traversed_Offset_Array'First) /= No_Field_Offset then
            while Offsets (Cur_Field + 1) /= No_Field_Offset loop
               declare
                  F : constant Union_Id :=
                    Get_Node_Field_Union (Cur_Node, Offsets (Cur_Field));

               begin
                  if Traverse_Field (F) = Abandon then
                     Pop_Parents;
                     return Abandon;
                  end if;
               end;

               Cur_Field := Cur_Field + 1;
            end loop;

            declare
               F : constant Union_Id :=
                 Get_Node_Field_Union (Cur_Node, Offsets (Cur_Field));

            begin
               if F not in Node_Range then
                  if Traverse_Field (F) = Abandon then
                     Pop_Parents;
                     return Abandon;
                  end if;

               elsif F /= Empty_List_Or_Node then
                  --  Here is the tail recursion step, we reset Cur_Node and
                  --  jump back to the start of the procedure, which has the
                  --  same semantic effect as a call.

                  Tail_Recursion_Counter := Tail_Recursion_Counter + 1;
                  Parent_Node := Cur_Node;
                  Cur_Node := Node_Id (F);
                  goto Tail_Recurse;
               end if;
            end;
         end if;
      end;

      Pop_Parents;
      return OK;
   end Internal_Traverse_With_Parent;

   -------------------
   -- Traverse_Func --
   -------------------

   function Traverse_Func (Node : Node_Id) return Traverse_Final_Result is
      pragma Debug (Validate_Node (Node));

      function Traverse_Field (Fld : Union_Id) return Traverse_Final_Result;
      --  Fld is one of the Traversed fields of Nod, which is necessarily a
      --  Node_Id or List_Id. It is traversed, and the result is the result of
      --  this traversal.

      --------------------
      -- Traverse_Field --
      --------------------

      function Traverse_Field (Fld : Union_Id) return Traverse_Final_Result is
      begin
         if Fld /= Union_Id (Empty) then

            --  Descendant is a node

            if Fld in Node_Range then
               return Traverse_Func (Node_Id (Fld));

            --  Descendant is a list

            elsif Fld in List_Range then
               declare
                  Elmt : Node_Id := First (List_Id (Fld));
               begin
                  while Present (Elmt) loop
                     if Traverse_Func (Elmt) = Abandon then
                        return Abandon;
                     end if;

                     Next (Elmt);
                  end loop;
               end;

            else
               raise Program_Error;
            end if;
         end if;

         return OK;
      end Traverse_Field;

      Cur_Node : Node_Id := Node;

   --  Start of processing for Traverse_Func

   begin
      --  If the last field is a node, we eliminate the tail recursion by
      --  jumping back to this label. This is because concatenations are
      --  sometimes deeply nested, as in X1&X2&...&Xn. Gen_IL ensures that the
      --  Left_Opnd field of N_Op_Concat comes last in Traversed_Fields, so the
      --  tail recursion is eliminated in that case. This trick prevents us
      --  from running out of stack memory in that case. We don't bother
      --  eliminating the tail recursion if the last field is a list.
      --
      --  (To check, look in the body of Sinfo.Nodes, search for the Left_Opnd
      --  getter, and note the offset of Left_Opnd. Then look in the spec of
      --  Sinfo.Nodes, look at the Traversed_Fields table, search for the
      --  N_Op_Concat component. The offset of Left_Opnd should be the last
      --  component before the No_Field_Offset sentinels.)

      <<Tail_Recurse>>

      case Process (Cur_Node) is
         when Abandon =>
            return Abandon;

         when Skip =>
            return OK;

         when OK =>
            null;

         when OK_Orig =>
            Cur_Node := Original_Node (Cur_Node);
      end case;

      --  Check for empty Traversed_Fields before entering loop below, so the
      --  tail recursive step won't go past the end.

      declare
         Cur_Field : Offset_Array_Index := Traversed_Offset_Array'First;
         Offsets : Traversed_Offset_Array renames
           Traversed_Fields (Nkind (Cur_Node));

      begin
         if Offsets (Traversed_Offset_Array'First) /= No_Field_Offset then
            while Offsets (Cur_Field + 1) /= No_Field_Offset loop
               declare
                  F : constant Union_Id :=
                    Get_Node_Field_Union (Cur_Node, Offsets (Cur_Field));

               begin
                  if Traverse_Field (F) = Abandon then
                     return Abandon;
                  end if;
               end;

               Cur_Field := Cur_Field + 1;
            end loop;

            declare
               F : constant Union_Id :=
                 Get_Node_Field_Union (Cur_Node, Offsets (Cur_Field));

            begin
               if F not in Node_Range then
                  if Traverse_Field (F) = Abandon then
                     return Abandon;
                  end if;

               elsif F /= Empty_List_Or_Node then
                  --  Here is the tail recursion step, we reset Cur_Node and
                  --  jump back to the start of the procedure, which has the
                  --  same semantic effect as a call.

                  Cur_Node := Node_Id (F);
                  goto Tail_Recurse;
               end if;
            end;
         end if;
      end;

      return OK;
   end Traverse_Func;

   -------------------------------
   -- Traverse_Func_With_Parent --
   -------------------------------

   function Traverse_Func_With_Parent
     (Node : Node_Id) return Traverse_Final_Result
   is
      function Traverse is new Internal_Traverse_With_Parent (Process);
      Result : Traverse_Final_Result;
   begin
      --  Ensure that the Parents stack is not currently in use; required since
      --  it is global and hence a tree traversal with parents must be finished
      --  before the next tree traversal with parents starts.

      pragma Assert (Parents_Stack.Last = 0);
      Parents_Stack.Set_Last (0);

      Parents_Stack.Append (Parent (Node));
      Result := Traverse (Node);
      Parents_Stack.Decrement_Last;

      pragma Assert (Parents_Stack.Last = 0);

      return Result;
   end Traverse_Func_With_Parent;

   -------------------
   -- Traverse_Proc --
   -------------------

   procedure Traverse_Proc (Node : Node_Id) is
      function Traverse is new Traverse_Func (Process);
      Discard : Traverse_Final_Result;
      pragma Warnings (Off, Discard);
   begin
      Discard := Traverse (Node);
   end Traverse_Proc;

   -------------------------------
   -- Traverse_Proc_With_Parent --
   -------------------------------

   procedure Traverse_Proc_With_Parent (Node : Node_Id) is
      function Traverse is new Traverse_Func_With_Parent (Process);
      Discard : Traverse_Final_Result;
      pragma Warnings (Off, Discard);
   begin
      Discard := Traverse (Node);
   end Traverse_Proc_With_Parent;

   ------------
   -- Unlock --
   ------------

   procedure Unlock is
   begin
      Orig_Nodes.Locked := False;
   end Unlock;

   ------------------
   -- Unlock_Nodes --
   ------------------

   procedure Unlock_Nodes is
   begin
      pragma Assert (Locked);
      Locked := False;
   end Unlock_Nodes;

   ----------------
   -- Zero_Slots --
   ----------------

   procedure Zero_Dynamic_Slots (First, Last : Node_Offset'Base) is
   begin
      Slots.Table (First .. Last) := (others => 0);
   end Zero_Dynamic_Slots;

   procedure Zero_Header_Slots (N : Node_Or_Entity_Id) is
      All_Node_Offsets : Node_Offsets.Table_Type renames
        Node_Offsets.Table (Node_Offsets.First .. Node_Offsets.Last);
   begin
      All_Node_Offsets (N).Slots := (others => 0);
   end Zero_Header_Slots;

   procedure Zero_Slots (N : Node_Or_Entity_Id) is
   begin
      Zero_Dynamic_Slots (Off_F (N), Off_L (N));
      Zero_Header_Slots (N);
   end Zero_Slots;

   ----------------------
   -- Print_Statistics --
   ----------------------

   procedure Print_Node_Statistics;
   procedure Print_Field_Statistics;
   --  Helpers for Print_Statistics

   procedure Write_Ratio (X : Nat_64; Y : Pos_64);
   --  Write the value of (X/Y) without using 'Image (approximately)

   procedure Write_Ratio (X : Nat_64; Y : Pos_64) is
      pragma Assert (X <= Y);
      Ratio : constant Nat := Nat ((Long_Float (X) / Long_Float (Y)) * 1000.0);
   begin
      Write_Str (" (");

      if Ratio = 0 then
         Write_Str ("0.000");
      elsif Ratio in 1 .. 9 then
         Write_Str ("0.00");
         Write_Int (Ratio);
      elsif Ratio in 10 .. 99 then
         Write_Str ("0.0");
         Write_Int (Ratio);
      elsif Ratio in 100 .. 999 then
         Write_Str ("0.");
         Write_Int (Ratio);
      else
         Write_Int (Ratio / 1000);
      end if;

      Write_Str (")");
   end Write_Ratio;

   procedure Print_Node_Statistics is
      subtype Count is Nat_64;
      Node_Counts : array (Node_Kind) of Count := (others => 0);
      Entity_Counts : array (Entity_Kind) of Count := (others => 0);

      All_Node_Offsets : Node_Offsets.Table_Type renames
        Node_Offsets.Table (Node_Offsets.First .. Node_Offsets.Last);
   begin
      Write_Int (Int (Node_Offsets.Last));
      Write_Line (" nodes (including entities)");
      Write_Int (Int (Slots.Last));
      Write_Line (" non-header slots");

      for N in All_Node_Offsets'Range loop
         declare
            K : constant Node_Kind := Nkind (N);

         begin
            Node_Counts (K) := Node_Counts (K) + 1;

            if K in N_Entity then
               Entity_Counts (Ekind (N)) := Entity_Counts (Ekind (N)) + 1;
            end if;
         end;
      end loop;

      for K in Node_Kind loop
         declare
            Count : constant Nat_64 := Node_Counts (K);
         begin
            Write_Int_64 (Count);
            Write_Ratio (Count, Int_64 (Node_Offsets.Last));
            Write_Str (" ");
            Write_Str (Node_Kind'Image (K));
            Write_Str (" ");
            Write_Int (Int (Sinfo.Nodes.Size (K)));
            Write_Str (" slots");
            Write_Eol;
         end;
      end loop;

      for K in Entity_Kind loop
         declare
            Count : constant Nat_64 := Entity_Counts (K);
         begin
            Write_Int_64 (Count);
            Write_Ratio (Count, Int_64 (Node_Offsets.Last));
            Write_Str (" ");
            Write_Str (Entity_Kind'Image (K));
            Write_Str (" ");
            Write_Int (Int (Einfo.Entities.Size (K)));
            Write_Str (" slots");
            Write_Eol;
         end;
      end loop;
   end Print_Node_Statistics;

   procedure Print_Field_Statistics is
      Total, G_Total, S_Total : Call_Count := 0;
   begin
      Write_Int_64 (Get_Original_Node_Count);
      Write_Str (" + ");
      Write_Int_64 (Set_Original_Node_Count);
      Write_Eol;
      Write_Line (" Original_Node_Count getter and setter calls");
      Write_Eol;

      Write_Line ("Frequency of field getter and setter calls:");

      for Field in Node_Or_Entity_Field loop
         G_Total := G_Total + Get_Count (Field);
         S_Total := S_Total + Set_Count (Field);
         Total := G_Total + S_Total;
      end loop;

      --  This assertion helps CodePeer understand that Total cannot be 0 (this
      --  is true because GNAT does not attempt to compile empty files).
      pragma Assert (Total > 0);

      Write_Int_64 (Total);
      Write_Str (" (100%) = ");
      Write_Int_64 (G_Total);
      Write_Str (" + ");
      Write_Int_64 (S_Total);
      Write_Line (" total getter and setter calls");

      for Field in Node_Or_Entity_Field loop
         declare
            G : constant Call_Count := Get_Count (Field);
            S : constant Call_Count := Set_Count (Field);
            GS : constant Call_Count := G + S;

            Desc : Field_Descriptor renames Field_Descriptors (Field);
            Slot : constant Field_Offset :=
              (Field_Size (Desc.Kind) * Desc.Offset) / Slot_Size;

         begin
            Write_Int_64 (GS);
            Write_Ratio (GS, Total);
            Write_Str (" = ");
            Write_Int_64 (G);
            Write_Str (" + ");
            Write_Int_64 (S);
            Write_Str (" ");
            Write_Str (Node_Or_Entity_Field'Image (Field));
            Write_Str (" in slot ");
            Write_Int (Int (Slot));
            Write_Str (" size ");
            Write_Int (Int (Field_Size (Desc.Kind)));
            Write_Eol;
         end;
      end loop;
   end Print_Field_Statistics;

   procedure Print_Statistics is
   begin
      Write_Eol;
      Write_Eol;
      Print_Node_Statistics;
      Print_Field_Statistics;
   end Print_Statistics;

end Atree;
