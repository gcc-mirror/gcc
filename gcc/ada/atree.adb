------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                A T R E E                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2013, Free Software Foundation, Inc.         --
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

pragma Style_Checks (All_Checks);
--  Turn off subprogram ordering check for this package

--  WARNING: There is a C version of this package. Any changes to this source
--  file must be properly reflected in the file atree.h which is a C header
--  file containing equivalent definitions for use by gigi.

with Aspects; use Aspects;
with Debug;   use Debug;
with Nlists;  use Nlists;
with Output;  use Output;
with Sinput;  use Sinput;
with Tree_IO; use Tree_IO;

package body Atree is

   Reporting_Proc : Report_Proc := null;
   --  Record argument to last call to Set_Reporting_Proc

   ---------------
   -- Debugging --
   ---------------

   --  Suppose you find that node 12345 is messed up. You might want to find
   --  the code that created that node. There are two ways to do this:

   --  One way is to set a conditional breakpoint on New_Node_Debugging_Output
   --  (nickname "nnd"):
   --     break nnd if n = 12345
   --  and run gnat1 again from the beginning.

   --  The other way is to set a breakpoint near the beginning (e.g. on
   --  gnat1drv), and run. Then set Watch_Node (nickname "ww") to 12345 in gdb:
   --     ww := 12345
   --  and set a breakpoint on New_Node_Breakpoint (nickname "nn"). Continue.

   --  Either way, gnat1 will stop when node 12345 is created

   --  The second method is much faster

   --  Similarly, rr and rrd allow breaking on rewriting of a given node

   ww : Node_Id'Base := Node_Id'First - 1;
   pragma Export (Ada, ww); --  trick the optimizer
   Watch_Node : Node_Id'Base renames ww;
   --  Node to "watch"; that is, whenever a node is created, we check if it
   --  is equal to Watch_Node, and if so, call New_Node_Breakpoint. You have
   --  presumably set a breakpoint on New_Node_Breakpoint. Note that the
   --  initial value of Node_Id'First - 1 ensures that by default, no node
   --  will be equal to Watch_Node.

   procedure nn;
   pragma Export (Ada, nn);
   procedure New_Node_Breakpoint renames nn;
   --  This doesn't do anything interesting; it's just for setting breakpoint
   --  on as explained above.

   procedure nnd (N : Node_Id);
   pragma Export (Ada, nnd);
   procedure New_Node_Debugging_Output (N : Node_Id) renames nnd;
   --  For debugging. If debugging is turned on, New_Node and New_Entity call
   --  this. If debug flag N is turned on, this prints out the new node.
   --
   --  If Node = Watch_Node, this prints out the new node and calls
   --  New_Node_Breakpoint. Otherwise, does nothing.

   procedure rr;
   pragma Export (Ada, rr);
   procedure Rewrite_Breakpoint renames rr;
   --  This doesn't do anything interesting; it's just for setting breakpoint
   --  on as explained above.

   procedure rrd (Old_Node, New_Node : Node_Id);
   pragma Export (Ada, rrd);
   procedure Rewrite_Debugging_Output
     (Old_Node, New_Node : Node_Id) renames rrd;
   --  For debugging. If debugging is turned on, Rewrite calls this. If debug
   --  flag N is turned on, this prints out the new node.
   --
   --  If Old_Node = Watch_Node, this prints out the old and new nodes and
   --  calls Rewrite_Breakpoint. Otherwise, does nothing.

   procedure Node_Debug_Output (Op : String; N : Node_Id);
   --  Common code for nnd and rrd, writes Op followed by information about N

   -----------------------------
   -- Local Objects and Types --
   -----------------------------

   Node_Count : Nat;
   --  Count allocated nodes for Num_Nodes function

   use Unchecked_Access;
   --  We are allowed to see these from within our own body

   use Atree_Private_Part;
   --  We are also allowed to see our private data structures

   --  Functions used to store Entity_Kind value in Nkind field

   --  The following declarations are used to store flags 65-72 in the
   --  Nkind field of the third component of an extended (entity) node.

   type Flag_Byte is record
      Flag65 : Boolean;
      Flag66 : Boolean;
      Flag67 : Boolean;
      Flag68 : Boolean;
      Flag69 : Boolean;
      Flag70 : Boolean;
      Flag71 : Boolean;
      Flag72 : Boolean;
   end record;

   pragma Pack (Flag_Byte);
   for Flag_Byte'Size use 8;

   type Flag_Byte_Ptr is access all Flag_Byte;
   type Node_Kind_Ptr is access all Node_Kind;

   function To_Flag_Byte is new
     Unchecked_Conversion (Node_Kind, Flag_Byte);

   function To_Flag_Byte_Ptr is new
     Unchecked_Conversion (Node_Kind_Ptr, Flag_Byte_Ptr);

   --  The following declarations are used to store flags 239-246 in the
   --  Nkind field of the fourth component of an extended (entity) node.

   type Flag_Byte2 is record
      Flag239 : Boolean;
      Flag240 : Boolean;
      Flag241 : Boolean;
      Flag242 : Boolean;
      Flag243 : Boolean;
      Flag244 : Boolean;
      Flag245 : Boolean;
      Flag246 : Boolean;
   end record;

   pragma Pack (Flag_Byte2);
   for Flag_Byte2'Size use 8;

   type Flag_Byte2_Ptr is access all Flag_Byte2;

   function To_Flag_Byte2 is new
     Unchecked_Conversion (Node_Kind, Flag_Byte2);

   function To_Flag_Byte2_Ptr is new
     Unchecked_Conversion (Node_Kind_Ptr, Flag_Byte2_Ptr);

   --  The following declarations are used to store flags 247-254 in the
   --  Nkind field of the fifth component of an extended (entity) node.

   type Flag_Byte3 is record
      Flag247 : Boolean;
      Flag248 : Boolean;
      Flag249 : Boolean;
      Flag250 : Boolean;
      Flag251 : Boolean;
      Flag252 : Boolean;
      Flag253 : Boolean;
      Flag254 : Boolean;
   end record;

   pragma Pack (Flag_Byte3);
   for Flag_Byte3'Size use 8;

   type Flag_Byte3_Ptr is access all Flag_Byte3;

   function To_Flag_Byte3 is new
     Unchecked_Conversion (Node_Kind, Flag_Byte3);

   function To_Flag_Byte3_Ptr is new
     Unchecked_Conversion (Node_Kind_Ptr, Flag_Byte3_Ptr);

   --  The following declarations are used to store flags 310-317 in the
   --  Nkind field of the sixth component of an extended (entity) node.

   type Flag_Byte4 is record
      Flag310 : Boolean;
      Flag311 : Boolean;
      Flag312 : Boolean;
      Flag313 : Boolean;
      Flag314 : Boolean;
      Flag315 : Boolean;
      Flag316 : Boolean;
      Flag317 : Boolean;
   end record;

   pragma Pack (Flag_Byte4);
   for Flag_Byte4'Size use 8;

   type Flag_Byte4_Ptr is access all Flag_Byte4;

   function To_Flag_Byte4 is new
     Unchecked_Conversion (Node_Kind, Flag_Byte4);

   function To_Flag_Byte4_Ptr is new
     Unchecked_Conversion (Node_Kind_Ptr, Flag_Byte4_Ptr);

   --  The following declarations are used to store flags 73-96 and the
   --  Convention field in the Field12 field of the third component of an
   --  extended (Entity) node.

   type Flag_Word is record
      Flag73 : Boolean;
      Flag74 : Boolean;
      Flag75 : Boolean;
      Flag76 : Boolean;
      Flag77 : Boolean;
      Flag78 : Boolean;
      Flag79 : Boolean;
      Flag80 : Boolean;

      Flag81 : Boolean;
      Flag82 : Boolean;
      Flag83 : Boolean;
      Flag84 : Boolean;
      Flag85 : Boolean;
      Flag86 : Boolean;
      Flag87 : Boolean;
      Flag88 : Boolean;

      Flag89 : Boolean;
      Flag90 : Boolean;
      Flag91 : Boolean;
      Flag92 : Boolean;
      Flag93 : Boolean;
      Flag94 : Boolean;
      Flag95 : Boolean;
      Flag96 : Boolean;

      Convention : Convention_Id;
   end record;

   pragma Pack (Flag_Word);
   for Flag_Word'Size use 32;
   for Flag_Word'Alignment use 4;

   type Flag_Word_Ptr is access all Flag_Word;
   type Union_Id_Ptr  is access all Union_Id;

   function To_Flag_Word is new
     Unchecked_Conversion (Union_Id, Flag_Word);

   function To_Flag_Word_Ptr is new
     Unchecked_Conversion (Union_Id_Ptr, Flag_Word_Ptr);

   --  The following declarations are used to store flags 97-128 in the
   --  Field12 field of the fourth component of an extended (entity) node.

   type Flag_Word2 is record
      Flag97  : Boolean;
      Flag98  : Boolean;
      Flag99  : Boolean;
      Flag100 : Boolean;
      Flag101 : Boolean;
      Flag102 : Boolean;
      Flag103 : Boolean;
      Flag104 : Boolean;

      Flag105 : Boolean;
      Flag106 : Boolean;
      Flag107 : Boolean;
      Flag108 : Boolean;
      Flag109 : Boolean;
      Flag110 : Boolean;
      Flag111 : Boolean;
      Flag112 : Boolean;

      Flag113 : Boolean;
      Flag114 : Boolean;
      Flag115 : Boolean;
      Flag116 : Boolean;
      Flag117 : Boolean;
      Flag118 : Boolean;
      Flag119 : Boolean;
      Flag120 : Boolean;

      Flag121 : Boolean;
      Flag122 : Boolean;
      Flag123 : Boolean;
      Flag124 : Boolean;
      Flag125 : Boolean;
      Flag126 : Boolean;
      Flag127 : Boolean;
      Flag128 : Boolean;
   end record;

   pragma Pack (Flag_Word2);
   for Flag_Word2'Size use 32;
   for Flag_Word2'Alignment use 4;

   type Flag_Word2_Ptr is access all Flag_Word2;

   function To_Flag_Word2 is new
     Unchecked_Conversion (Union_Id, Flag_Word2);

   function To_Flag_Word2_Ptr is new
     Unchecked_Conversion (Union_Id_Ptr, Flag_Word2_Ptr);

   --  The following declarations are used to store flags 152-183 in the
   --  Field11 field of the fourth component of an extended (entity) node.

   type Flag_Word3 is record
      Flag152 : Boolean;
      Flag153 : Boolean;
      Flag154 : Boolean;
      Flag155 : Boolean;
      Flag156 : Boolean;
      Flag157 : Boolean;
      Flag158 : Boolean;
      Flag159 : Boolean;

      Flag160 : Boolean;
      Flag161 : Boolean;
      Flag162 : Boolean;
      Flag163 : Boolean;
      Flag164 : Boolean;
      Flag165 : Boolean;
      Flag166 : Boolean;
      Flag167 : Boolean;

      Flag168 : Boolean;
      Flag169 : Boolean;
      Flag170 : Boolean;
      Flag171 : Boolean;
      Flag172 : Boolean;
      Flag173 : Boolean;
      Flag174 : Boolean;
      Flag175 : Boolean;

      Flag176 : Boolean;
      Flag177 : Boolean;
      Flag178 : Boolean;
      Flag179 : Boolean;
      Flag180 : Boolean;
      Flag181 : Boolean;
      Flag182 : Boolean;
      Flag183 : Boolean;
   end record;

   pragma Pack (Flag_Word3);
   for Flag_Word3'Size use 32;
   for Flag_Word3'Alignment use 4;

   type Flag_Word3_Ptr is access all Flag_Word3;

   function To_Flag_Word3 is new
     Unchecked_Conversion (Union_Id, Flag_Word3);

   function To_Flag_Word3_Ptr is new
     Unchecked_Conversion (Union_Id_Ptr, Flag_Word3_Ptr);

   --  The following declarations are used to store flags 184-215 in the
   --  Field12 field of the fifth component of an extended (entity) node.

   type Flag_Word4 is record
      Flag184 : Boolean;
      Flag185 : Boolean;
      Flag186 : Boolean;
      Flag187 : Boolean;
      Flag188 : Boolean;
      Flag189 : Boolean;
      Flag190 : Boolean;
      Flag191 : Boolean;

      Flag192 : Boolean;
      Flag193 : Boolean;
      Flag194 : Boolean;
      Flag195 : Boolean;
      Flag196 : Boolean;
      Flag197 : Boolean;
      Flag198 : Boolean;
      Flag199 : Boolean;

      Flag200 : Boolean;
      Flag201 : Boolean;
      Flag202 : Boolean;
      Flag203 : Boolean;
      Flag204 : Boolean;
      Flag205 : Boolean;
      Flag206 : Boolean;
      Flag207 : Boolean;

      Flag208 : Boolean;
      Flag209 : Boolean;
      Flag210 : Boolean;
      Flag211 : Boolean;
      Flag212 : Boolean;
      Flag213 : Boolean;
      Flag214 : Boolean;
      Flag215 : Boolean;
   end record;

   pragma Pack (Flag_Word4);
   for Flag_Word4'Size use 32;
   for Flag_Word4'Alignment use 4;

   type Flag_Word4_Ptr is access all Flag_Word4;

   function To_Flag_Word4 is new
     Unchecked_Conversion (Union_Id, Flag_Word4);

   function To_Flag_Word4_Ptr is new
     Unchecked_Conversion (Union_Id_Ptr, Flag_Word4_Ptr);

   --  The following declarations are used to store flags 255-286 in the
   --  Field12 field of the sixth component of an extended (entity) node.

   type Flag_Word5 is record
      Flag255 : Boolean;
      Flag256 : Boolean;
      Flag257 : Boolean;
      Flag258 : Boolean;
      Flag259 : Boolean;
      Flag260 : Boolean;
      Flag261 : Boolean;
      Flag262 : Boolean;

      Flag263 : Boolean;
      Flag264 : Boolean;
      Flag265 : Boolean;
      Flag266 : Boolean;
      Flag267 : Boolean;
      Flag268 : Boolean;
      Flag269 : Boolean;
      Flag270 : Boolean;

      Flag271 : Boolean;
      Flag272 : Boolean;
      Flag273 : Boolean;
      Flag274 : Boolean;
      Flag275 : Boolean;
      Flag276 : Boolean;
      Flag277 : Boolean;
      Flag278 : Boolean;

      Flag279 : Boolean;
      Flag280 : Boolean;
      Flag281 : Boolean;
      Flag282 : Boolean;
      Flag283 : Boolean;
      Flag284 : Boolean;
      Flag285 : Boolean;
      Flag286 : Boolean;
   end record;

   pragma Pack (Flag_Word5);
   for Flag_Word5'Size use 32;
   for Flag_Word5'Alignment use 4;

   type Flag_Word5_Ptr is access all Flag_Word5;

   function To_Flag_Word5 is new
     Unchecked_Conversion (Union_Id, Flag_Word5);

   function To_Flag_Word5_Ptr is new
     Unchecked_Conversion (Union_Id_Ptr, Flag_Word5_Ptr);

   --------------------------------------------------
   -- Implementation of Tree Substitution Routines --
   --------------------------------------------------

   --  A separate table keeps track of the mapping between rewritten nodes
   --  and their corresponding original tree nodes. Rewrite makes an entry
   --  in this table for use by Original_Node. By default, if no call is
   --  Rewrite, the entry in this table points to the original unwritten node.

   --  Note: eventually, this should be a field in the Node directly, but
   --  for now we do not want to disturb the efficiency of a power of 2
   --  for the node size

   package Orig_Nodes is new Table.Table (
      Table_Component_Type => Node_Id,
      Table_Index_Type     => Node_Id'Base,
      Table_Low_Bound      => First_Node_Id,
      Table_Initial        => Alloc.Orig_Nodes_Initial,
      Table_Increment      => Alloc.Orig_Nodes_Increment,
      Table_Name           => "Orig_Nodes");

   --------------------------
   -- Paren_Count Handling --
   --------------------------

   --  As noted in the spec, the paren count in a sub-expression node has
   --  four possible values 0,1,2, and 3. The value 3 really means 3 or more,
   --  and we use an auxiliary serially scanned table to record the actual
   --  count. A serial search is fine, only pathological programs will use
   --  entries in this table. Normal programs won't use it at all.

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

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Fix_Parents (Ref_Node, Fix_Node : Node_Id);
   --  Fixup parent pointers for the syntactic children of Fix_Node after
   --  a copy, setting them to Fix_Node when they pointed to Ref_Node.

   function Allocate_Initialize_Node
     (Src            : Node_Id;
      With_Extension : Boolean) return Node_Id;
   --  Allocate a new node or node extension. If Src is not empty, the
   --  information for the newly-allocated node is copied from it.

   ------------------------------
   -- Allocate_Initialize_Node --
   ------------------------------

   function Allocate_Initialize_Node
     (Src            : Node_Id;
      With_Extension : Boolean) return Node_Id
   is
      New_Id : Node_Id;

   begin
      if Present (Src)
        and then not Has_Extension (Src)
        and then With_Extension
        and then Src = Nodes.Last
      then
         New_Id := Src;

      else
         --  We are allocating a new node, or extending a node
         --  other than Nodes.Last.

         if Present (Src) then
            Nodes.Append (Nodes.Table (Src));
            Flags.Append (Flags.Table (Src));
         else
            Nodes.Append (Default_Node);
            Flags.Append (Default_Flags);
         end if;

         New_Id := Nodes.Last;
         Orig_Nodes.Append (New_Id);
         Node_Count := Node_Count + 1;
      end if;

      --  Specifically copy Paren_Count to deal with creating new table entry
      --  if the parentheses count is at the maximum possible value already.

      if Present (Src) and then Nkind (Src) in N_Subexpr then
         Set_Paren_Count (New_Id, Paren_Count (Src));
      end if;

      --  Set extension nodes if required

      if With_Extension then
         if Present (Src) and then Has_Extension (Src) then
            for J in 1 .. Num_Extension_Nodes loop
               Nodes.Append (Nodes.Table (Src + J));
               Flags.Append (Flags.Table (Src + J));
            end loop;
         else
            for J in 1 .. Num_Extension_Nodes loop
               Nodes.Append (Default_Node_Extension);
               Flags.Append (Default_Flags);
            end loop;
         end if;
      end if;

      Orig_Nodes.Set_Last (Nodes.Last);
      Allocate_List_Tables (Nodes.Last);

      --  Invoke the reporting procedure (if available)

      if Reporting_Proc /= null then
         Reporting_Proc.all (Target => New_Id, Source => Src);
      end if;

      return New_Id;
   end Allocate_Initialize_Node;

   --------------
   -- Analyzed --
   --------------

   function Analyzed (N : Node_Id) return Boolean is
   begin
      pragma Assert (N <= Nodes.Last);
      return Nodes.Table (N).Analyzed;
   end Analyzed;

   --------------------------
   -- Basic_Set_Convention --
   --------------------------

   procedure Basic_Set_Convention  (E : Entity_Id; Val : Convention_Id) is
   begin
      pragma Assert (Nkind (E) in N_Entity);
      To_Flag_Word_Ptr
        (Union_Id_Ptr'
          (Nodes.Table (E + 2).Field12'Unrestricted_Access)).Convention := Val;
   end Basic_Set_Convention;

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

   procedure Change_Node (N : Node_Id; New_Node_Kind : Node_Kind) is
      Save_Sloc    : constant Source_Ptr := Sloc (N);
      Save_In_List : constant Boolean    := Nodes.Table (N).In_List;
      Save_Link    : constant Union_Id   := Nodes.Table (N).Link;
      Save_CFS     : constant Boolean    := Nodes.Table (N).Comes_From_Source;
      Save_Posted  : constant Boolean    := Nodes.Table (N).Error_Posted;
      Par_Count    : Nat                 := 0;

   begin
      if Nkind (N) in N_Subexpr then
         Par_Count := Paren_Count (N);
      end if;

      Nodes.Table (N)                   := Default_Node;
      Nodes.Table (N).Sloc              := Save_Sloc;
      Nodes.Table (N).In_List           := Save_In_List;
      Nodes.Table (N).Link              := Save_Link;
      Nodes.Table (N).Comes_From_Source := Save_CFS;
      Nodes.Table (N).Nkind             := New_Node_Kind;
      Nodes.Table (N).Error_Posted      := Save_Posted;

      Flags.Table (N) := Default_Flags;

      if New_Node_Kind in N_Subexpr then
         Set_Paren_Count (N, Par_Count);
      end if;
   end Change_Node;

   -----------------------
   -- Comes_From_Source --
   -----------------------

   function Comes_From_Source (N : Node_Id) return Boolean is
   begin
      pragma Assert (N <= Nodes.Last);
      return Nodes.Table (N).Comes_From_Source;
   end Comes_From_Source;

   ----------------
   -- Convention --
   ----------------

   function Convention (E : Entity_Id) return Convention_Id is
   begin
      pragma Assert (Nkind (E) in N_Entity);
      return To_Flag_Word (Nodes.Table (E + 2).Field12).Convention;
   end Convention;

   ---------------
   -- Copy_Node --
   ---------------

   procedure Copy_Node (Source : Node_Id; Destination : Node_Id) is
      Save_In_List : constant Boolean  := Nodes.Table (Destination).In_List;
      Save_Link    : constant Union_Id := Nodes.Table (Destination).Link;

   begin
      Nodes.Table (Destination)         := Nodes.Table (Source);
      Nodes.Table (Destination).In_List := Save_In_List;
      Nodes.Table (Destination).Link    := Save_Link;

      Flags.Table (Destination) := Flags.Table (Source);

      --  Specifically set Paren_Count to make sure auxiliary table entry
      --  gets correctly made if the parentheses count is at the max value.

      if Nkind (Destination) in N_Subexpr then
         Set_Paren_Count (Destination, Paren_Count (Source));
      end if;

      --  Deal with copying extension nodes if present. No need to copy flags
      --  table entries, since they are always zero for extending components.

      if Has_Extension (Source) then
         pragma Assert (Has_Extension (Destination));

         for J in 1 .. Num_Extension_Nodes loop
            Nodes.Table (Destination + J) := Nodes.Table (Source + J);
         end loop;

      else
         pragma Assert (not Has_Extension (Source));
         null;
      end if;
   end Copy_Node;

   ------------------------
   -- Copy_Separate_List --
   ------------------------

   function Copy_Separate_List (Source : List_Id) return List_Id is
      Result : constant List_Id := New_List;
      Nod    : Node_Id;

   begin
      Nod := First (Source);
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
      New_Id  : Node_Id;

      function Copy_Entity (E : Entity_Id) return Entity_Id;
      --  Copy Entity, copying only the Ekind and Chars fields

      function Copy_List (List : List_Id) return List_Id;
      --  Copy list

      function Possible_Copy (Field : Union_Id) return Union_Id;
      --  Given a field, returns a copy of the node or list if its parent
      --  is the current source node, and otherwise returns the input

      -----------------
      -- Copy_Entity --
      -----------------

      function Copy_Entity (E : Entity_Id) return Entity_Id is
         New_Ent : Entity_Id;

      begin
         case N_Entity (Nkind (E)) is
            when N_Defining_Identifier =>
               New_Ent := New_Entity (N_Defining_Identifier, Sloc (E));

            when N_Defining_Character_Literal =>
               New_Ent := New_Entity (N_Defining_Character_Literal, Sloc (E));

            when N_Defining_Operator_Symbol =>
               New_Ent := New_Entity (N_Defining_Operator_Symbol, Sloc (E));
         end case;

         Set_Chars (New_Ent, Chars (E));
         return New_Ent;
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
               if Has_Extension (E) then
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

            if Parent (Node_Id (Field)) = Source then
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

   --  Start of processing for Copy_Separate_Tree

   begin
      if Source <= Empty_Or_Error then
         return Source;

      elsif Has_Extension (Source) then
         return Copy_Entity (Source);

      else
         New_Id := New_Copy (Source);

         --  Recursively copy descendents

         Set_Field1 (New_Id, Possible_Copy (Field1 (New_Id)));
         Set_Field2 (New_Id, Possible_Copy (Field2 (New_Id)));
         Set_Field3 (New_Id, Possible_Copy (Field3 (New_Id)));
         Set_Field4 (New_Id, Possible_Copy (Field4 (New_Id)));
         Set_Field5 (New_Id, Possible_Copy (Field5 (New_Id)));

         --  Set Entity field to Empty to ensure that no entity references
         --  are shared between the two, if the source is already analyzed.

         if Nkind (New_Id) in N_Has_Entity
           or else Nkind (New_Id) = N_Freeze_Entity
         then
            Set_Entity (New_Id, Empty);
         end if;

         --  All done, return copied node

         return New_Id;
      end if;
   end Copy_Separate_Tree;

   -----------
   -- Ekind --
   -----------

   function Ekind (E : Entity_Id) return Entity_Kind is
   begin
      pragma Assert (Nkind (E) in N_Entity);
      return N_To_E (Nodes.Table (E + 1).Nkind);
   end Ekind;

   --------------
   -- Ekind_In --
   --------------

   function Ekind_In
     (T  : Entity_Kind;
      V1 : Entity_Kind;
      V2 : Entity_Kind) return Boolean
   is
   begin
      return T = V1 or else
             T = V2;
   end Ekind_In;

   function Ekind_In
     (T  : Entity_Kind;
      V1 : Entity_Kind;
      V2 : Entity_Kind;
      V3 : Entity_Kind) return Boolean
   is
   begin
      return T = V1 or else
             T = V2 or else
             T = V3;
   end Ekind_In;

   function Ekind_In
     (T  : Entity_Kind;
      V1 : Entity_Kind;
      V2 : Entity_Kind;
      V3 : Entity_Kind;
      V4 : Entity_Kind) return Boolean
   is
   begin
      return T = V1 or else
             T = V2 or else
             T = V3 or else
             T = V4;
   end Ekind_In;

   function Ekind_In
     (T  : Entity_Kind;
      V1 : Entity_Kind;
      V2 : Entity_Kind;
      V3 : Entity_Kind;
      V4 : Entity_Kind;
      V5 : Entity_Kind) return Boolean
   is
   begin
      return T = V1 or else
             T = V2 or else
             T = V3 or else
             T = V4 or else
             T = V5;
   end Ekind_In;

   function Ekind_In
     (T  : Entity_Kind;
      V1 : Entity_Kind;
      V2 : Entity_Kind;
      V3 : Entity_Kind;
      V4 : Entity_Kind;
      V5 : Entity_Kind;
      V6 : Entity_Kind) return Boolean
   is
   begin
      return T = V1 or else
             T = V2 or else
             T = V3 or else
             T = V4 or else
             T = V5 or else
             T = V6;
   end Ekind_In;

   function Ekind_In
     (T  : Entity_Kind;
      V1 : Entity_Kind;
      V2 : Entity_Kind;
      V3 : Entity_Kind;
      V4 : Entity_Kind;
      V5 : Entity_Kind;
      V6 : Entity_Kind;
      V7 : Entity_Kind) return Boolean
   is
   begin
      return T = V1 or else
             T = V2 or else
             T = V3 or else
             T = V4 or else
             T = V5 or else
             T = V6 or else
             T = V7;
   end Ekind_In;

   function Ekind_In
     (T  : Entity_Kind;
      V1 : Entity_Kind;
      V2 : Entity_Kind;
      V3 : Entity_Kind;
      V4 : Entity_Kind;
      V5 : Entity_Kind;
      V6 : Entity_Kind;
      V7 : Entity_Kind;
      V8 : Entity_Kind) return Boolean
   is
   begin
      return T = V1 or else
             T = V2 or else
             T = V3 or else
             T = V4 or else
             T = V5 or else
             T = V6 or else
             T = V7 or else
             T = V8;
   end Ekind_In;

   function Ekind_In
     (E  : Entity_Id;
      V1 : Entity_Kind;
      V2 : Entity_Kind) return Boolean
   is
   begin
      return Ekind_In (Ekind (E), V1, V2);
   end Ekind_In;

   function Ekind_In
     (E  : Entity_Id;
      V1 : Entity_Kind;
      V2 : Entity_Kind;
      V3 : Entity_Kind) return Boolean
   is
   begin
      return Ekind_In (Ekind (E), V1, V2, V3);
   end Ekind_In;

   function Ekind_In
     (E  : Entity_Id;
      V1 : Entity_Kind;
      V2 : Entity_Kind;
      V3 : Entity_Kind;
      V4 : Entity_Kind) return Boolean
   is
   begin
      return Ekind_In (Ekind (E), V1, V2, V3, V4);
   end Ekind_In;

   function Ekind_In
     (E  : Entity_Id;
      V1 : Entity_Kind;
      V2 : Entity_Kind;
      V3 : Entity_Kind;
      V4 : Entity_Kind;
      V5 : Entity_Kind) return Boolean
   is
   begin
      return Ekind_In (Ekind (E), V1, V2, V3, V4, V5);
   end Ekind_In;

   function Ekind_In
     (E  : Entity_Id;
      V1 : Entity_Kind;
      V2 : Entity_Kind;
      V3 : Entity_Kind;
      V4 : Entity_Kind;
      V5 : Entity_Kind;
      V6 : Entity_Kind) return Boolean
   is
   begin
      return Ekind_In (Ekind (E), V1, V2, V3, V4, V5, V6);
   end Ekind_In;

   function Ekind_In
     (E  : Entity_Id;
      V1 : Entity_Kind;
      V2 : Entity_Kind;
      V3 : Entity_Kind;
      V4 : Entity_Kind;
      V5 : Entity_Kind;
      V6 : Entity_Kind;
      V7 : Entity_Kind) return Boolean
   is
   begin
      return Ekind_In (Ekind (E), V1, V2, V3, V4, V5, V6, V7);
   end Ekind_In;

   function Ekind_In
     (E  : Entity_Id;
      V1 : Entity_Kind;
      V2 : Entity_Kind;
      V3 : Entity_Kind;
      V4 : Entity_Kind;
      V5 : Entity_Kind;
      V6 : Entity_Kind;
      V7 : Entity_Kind;
      V8 : Entity_Kind) return Boolean
   is
   begin
      return Ekind_In (Ekind (E), V1, V2, V3, V4, V5, V6, V7, V8);
   end Ekind_In;

   ------------------------
   -- Set_Reporting_Proc --
   ------------------------

   procedure Set_Reporting_Proc (P : Report_Proc) is
   begin
      pragma Assert (Reporting_Proc = null);
      Reporting_Proc := P;
   end Set_Reporting_Proc;

   ------------------
   -- Error_Posted --
   ------------------

   function Error_Posted (N : Node_Id) return Boolean is
   begin
      pragma Assert (N <= Nodes.Last);
      return Nodes.Table (N).Error_Posted;
   end Error_Posted;

   -----------------------
   -- Exchange_Entities --
   -----------------------

   procedure Exchange_Entities (E1 : Entity_Id; E2 : Entity_Id) is
      Temp_Ent : Node_Record;
      Temp_Flg : Flags_Byte;

   begin
      pragma Assert (True
        and then Has_Extension (E1)
        and then Has_Extension (E2)
        and then not Nodes.Table (E1).In_List
        and then not Nodes.Table (E2).In_List);

      --  Exchange the contents of the two entities

      for J in 0 .. Num_Extension_Nodes loop
         Temp_Ent := Nodes.Table (E1 + J);
         Nodes.Table (E1 + J) := Nodes.Table (E2 + J);
         Nodes.Table (E2 + J) := Temp_Ent;
      end loop;

      --  Exchange flag bytes for first component. No need to do the exchange
      --  for the other components, since the flag bytes are always zero.

      Temp_Flg := Flags.Table (E1);
      Flags.Table (E1) := Flags.Table (E2);
      Flags.Table (E2) := Temp_Flg;

      --  That exchange exchanged the parent pointers as well, which is what
      --  we want, but we need to patch up the defining identifier pointers
      --  in the parent nodes (the child pointers) to match this switch
      --  unless for Implicit types entities which have no parent, in which
      --  case we don't do anything otherwise we won't be able to revert back
      --  to the original situation.

      --  Shouldn't this use Is_Itype instead of the Parent test

      if Present (Parent (E1)) and then Present (Parent (E2)) then
         Set_Defining_Identifier (Parent (E1), E1);
         Set_Defining_Identifier (Parent (E2), E2);
      end if;
   end Exchange_Entities;

   -----------------
   -- Extend_Node --
   -----------------

   function Extend_Node (Node : Node_Id) return Entity_Id is
      Result : Entity_Id;

      procedure Debug_Extend_Node;
      pragma Inline (Debug_Extend_Node);
      --  Debug routine for debug flag N

      -----------------------
      -- Debug_Extend_Node --
      -----------------------

      procedure Debug_Extend_Node is
      begin
         if Debug_Flag_N then
            Write_Str ("Extend node ");
            Write_Int (Int (Node));

            if Result = Node then
               Write_Str (" in place");
            else
               Write_Str (" copied to ");
               Write_Int (Int (Result));
            end if;

            --  Write_Eol;
         end if;
      end Debug_Extend_Node;

   --  Start of processing for Extend_Node

   begin
      pragma Assert (not (Has_Extension (Node)));
      Result := Allocate_Initialize_Node (Node, With_Extension => True);
      pragma Debug (Debug_Extend_Node);
      return Result;
   end Extend_Node;

   -----------------
   -- Fix_Parents --
   -----------------

   procedure Fix_Parents (Ref_Node, Fix_Node : Node_Id) is

      procedure Fix_Parent (Field : Union_Id);
      --  Fixup one parent pointer. Field is checked to see if it points to
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
           and then not Nodes.Table (Node_Id (Field)).In_List
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

   --  Start of processing for Fix_Parents

   begin
      Fix_Parent (Field1 (Fix_Node));
      Fix_Parent (Field2 (Fix_Node));
      Fix_Parent (Field3 (Fix_Node));
      Fix_Parent (Field4 (Fix_Node));
      Fix_Parent (Field5 (Fix_Node));
   end Fix_Parents;

   -------------------
   -- Flags_Address --
   -------------------

   function Flags_Address return System.Address is
   begin
      return Flags.Table (First_Node_Id)'Address;
   end Flags_Address;

   -----------------------------------
   -- Get_Comes_From_Source_Default --
   -----------------------------------

   function Get_Comes_From_Source_Default return Boolean is
   begin
      return Default_Node.Comes_From_Source;
   end Get_Comes_From_Source_Default;

   -----------------
   -- Has_Aspects --
   -----------------

   function Has_Aspects (N : Node_Id) return Boolean is
   begin
      pragma Assert (N <= Nodes.Last);
      return Nodes.Table (N).Has_Aspects;
   end Has_Aspects;

   -------------------
   -- Has_Extension --
   -------------------

   function Has_Extension (N : Node_Id) return Boolean is
   begin
      return N < Nodes.Last and then Nodes.Table (N + 1).Is_Extension;
   end Has_Extension;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Dummy : Node_Id;
      pragma Warnings (Off, Dummy);

   begin
      Node_Count := 0;
      Atree_Private_Part.Nodes.Init;
      Atree_Private_Part.Flags.Init;
      Orig_Nodes.Init;
      Paren_Counts.Init;

      --  Allocate Empty node

      Dummy := New_Node (N_Empty, No_Location);
      Set_Name1 (Empty, No_Name);

      --  Allocate Error node, and set Error_Posted, since we certainly
      --  only generate an Error node if we do post some kind of error.

      Dummy := New_Node (N_Error, No_Location);
      Set_Name1 (Error, Error_Name);
      Set_Error_Posted (Error, True);
   end Initialize;

   --------------------------
   -- Is_Rewrite_Insertion --
   --------------------------

   function Is_Rewrite_Insertion (Node : Node_Id) return Boolean is
   begin
      return Nodes.Table (Node).Rewrite_Ins;
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
      return Nodes.Last;
   end Last_Node_Id;

   ----------
   -- Lock --
   ----------

   procedure Lock is
   begin
      Nodes.Locked := True;
      Flags.Locked := True;
      Orig_Nodes.Locked := True;
      Nodes.Release;
      Flags.Release;
      Orig_Nodes.Release;
   end Lock;

   ----------------------------
   -- Mark_Rewrite_Insertion --
   ----------------------------

   procedure Mark_Rewrite_Insertion (New_Node : Node_Id) is
   begin
      Nodes.Table (New_Node).Rewrite_Ins := True;
   end Mark_Rewrite_Insertion;

   --------------
   -- New_Copy --
   --------------

   function New_Copy (Source : Node_Id) return Node_Id is
      New_Id : Node_Id := Source;

   begin
      if Source > Empty_Or_Error then
         New_Id := Allocate_Initialize_Node (Source, Has_Extension (Source));

         Nodes.Table (New_Id).Link := Empty_List_Or_Node;
         Nodes.Table (New_Id).In_List := False;

         --  If the original is marked as a rewrite insertion, then unmark the
         --  copy, since we inserted the original, not the copy.

         Nodes.Table (New_Id).Rewrite_Ins := False;
         pragma Debug (New_Node_Debugging_Output (New_Id));

         --  Clear Is_Overloaded since we cannot have semantic interpretations
         --  of this new node.

         if Nkind (Source) in N_Subexpr then
            Set_Is_Overloaded (New_Id, False);
         end if;

         --  Always clear Has_Aspects, the caller must take care of copying
         --  aspects if this is required for the particular situation.

         Set_Has_Aspects (New_Id, False);
      end if;

      return New_Id;
   end New_Copy;

   ----------------
   -- New_Entity --
   ----------------

   function New_Entity
     (New_Node_Kind : Node_Kind;
      New_Sloc      : Source_Ptr) return Entity_Id
   is
      Ent : Entity_Id;

   begin
      pragma Assert (New_Node_Kind in N_Entity);

      Ent := Allocate_Initialize_Node (Empty, With_Extension => True);

      --  If this is a node with a real location and we are generating
      --  source nodes, then reset Current_Error_Node. This is useful
      --  if we bomb during parsing to get a error location for the bomb.

      if Default_Node.Comes_From_Source and then New_Sloc > No_Location then
         Current_Error_Node := Ent;
      end if;

      Nodes.Table (Ent).Nkind  := New_Node_Kind;
      Nodes.Table (Ent).Sloc   := New_Sloc;
      pragma Debug (New_Node_Debugging_Output (Ent));

      return Ent;
   end New_Entity;

   --------------
   -- New_Node --
   --------------

   function New_Node
     (New_Node_Kind : Node_Kind;
      New_Sloc      : Source_Ptr) return Node_Id
   is
      Nod : Node_Id;

   begin
      pragma Assert (New_Node_Kind not in N_Entity);
      Nod := Allocate_Initialize_Node (Empty, With_Extension => False);
      Nodes.Table (Nod).Nkind := New_Node_Kind;
      Nodes.Table (Nod).Sloc  := New_Sloc;
      pragma Debug (New_Node_Debugging_Output (Nod));

      --  If this is a node with a real location and we are generating source
      --  nodes, then reset Current_Error_Node. This is useful if we bomb
      --  during parsing to get an error location for the bomb.

      if Default_Node.Comes_From_Source and then New_Sloc > No_Location then
         Current_Error_Node := Nod;
      end if;

      return Nod;
   end New_Node;

   -------------------------
   -- New_Node_Breakpoint --
   -------------------------

   procedure nn is
   begin
      Write_Str ("Watched node ");
      Write_Int (Int (Watch_Node));
      Write_Str (" created");
      Write_Eol;
   end nn;

   -------------------------------
   -- New_Node_Debugging_Output --
   -------------------------------

   procedure nnd (N : Node_Id) is
      Node_Is_Watched : constant Boolean := N = Watch_Node;

   begin
      if Debug_Flag_N or else Node_Is_Watched then
         Node_Debug_Output ("Allocate", N);

         if Node_Is_Watched then
            New_Node_Breakpoint;
         end if;
      end if;
   end nnd;

   -----------
   -- Nkind --
   -----------

   function Nkind (N : Node_Id) return Node_Kind is
   begin
      return Nodes.Table (N).Nkind;
   end Nkind;

   --------------
   -- Nkind_In --
   --------------

   function Nkind_In
     (N  : Node_Id;
      V1 : Node_Kind;
      V2 : Node_Kind) return Boolean
   is
   begin
      return Nkind_In (Nkind (N), V1, V2);
   end Nkind_In;

   function Nkind_In
     (N  : Node_Id;
      V1 : Node_Kind;
      V2 : Node_Kind;
      V3 : Node_Kind) return Boolean
   is
   begin
      return Nkind_In (Nkind (N), V1, V2, V3);
   end Nkind_In;

   function Nkind_In
     (N  : Node_Id;
      V1 : Node_Kind;
      V2 : Node_Kind;
      V3 : Node_Kind;
      V4 : Node_Kind) return Boolean
   is
   begin
      return Nkind_In (Nkind (N), V1, V2, V3, V4);
   end Nkind_In;

   function Nkind_In
     (N  : Node_Id;
      V1 : Node_Kind;
      V2 : Node_Kind;
      V3 : Node_Kind;
      V4 : Node_Kind;
      V5 : Node_Kind) return Boolean
   is
   begin
      return Nkind_In (Nkind (N), V1, V2, V3, V4, V5);
   end Nkind_In;

   function Nkind_In
     (N  : Node_Id;
      V1 : Node_Kind;
      V2 : Node_Kind;
      V3 : Node_Kind;
      V4 : Node_Kind;
      V5 : Node_Kind;
      V6 : Node_Kind) return Boolean
   is
   begin
      return Nkind_In (Nkind (N), V1, V2, V3, V4, V5, V6);
   end Nkind_In;

   function Nkind_In
     (N  : Node_Id;
      V1 : Node_Kind;
      V2 : Node_Kind;
      V3 : Node_Kind;
      V4 : Node_Kind;
      V5 : Node_Kind;
      V6 : Node_Kind;
      V7 : Node_Kind) return Boolean
   is
   begin
      return Nkind_In (Nkind (N), V1, V2, V3, V4, V5, V6, V7);
   end Nkind_In;

   function Nkind_In
     (N  : Node_Id;
      V1 : Node_Kind;
      V2 : Node_Kind;
      V3 : Node_Kind;
      V4 : Node_Kind;
      V5 : Node_Kind;
      V6 : Node_Kind;
      V7 : Node_Kind;
      V8 : Node_Kind) return Boolean
   is
   begin
      return Nkind_In (Nkind (N), V1, V2, V3, V4, V5, V6, V7, V8);
   end Nkind_In;

   function Nkind_In
     (N  : Node_Id;
      V1 : Node_Kind;
      V2 : Node_Kind;
      V3 : Node_Kind;
      V4 : Node_Kind;
      V5 : Node_Kind;
      V6 : Node_Kind;
      V7 : Node_Kind;
      V8 : Node_Kind;
      V9 : Node_Kind) return Boolean
   is
   begin
      return Nkind_In (Nkind (N), V1, V2, V3, V4, V5, V6, V7, V8, V9);
   end Nkind_In;

   --------
   -- No --
   --------

   function No (N : Node_Id) return Boolean is
   begin
      return N = Empty;
   end No;

   -----------------------
   -- Node_Debug_Output --
   -----------------------

   procedure Node_Debug_Output (Op : String; N : Node_Id) is
   begin
      Write_Str (Op);

      if Nkind (N) in N_Entity then
         Write_Str (" entity");
      else
         Write_Str (" node");
      end if;

      Write_Str (" Id = ");
      Write_Int (Int (N));
      Write_Str ("  ");
      Write_Location (Sloc (N));
      Write_Str ("  ");
      Write_Str (Node_Kind'Image (Nkind (N)));
      Write_Eol;
   end Node_Debug_Output;

   -------------------
   -- Nodes_Address --
   -------------------

   function Nodes_Address return System.Address is
   begin
      return Nodes.Table (First_Node_Id)'Address;
   end Nodes_Address;

   ---------------
   -- Num_Nodes --
   ---------------

   function Num_Nodes return Nat is
   begin
      return Node_Count;
   end Num_Nodes;

   -------------------
   -- Original_Node --
   -------------------

   function Original_Node (Node : Node_Id) return Node_Id is
   begin
      return Orig_Nodes.Table (Node);
   end Original_Node;

   -----------------
   -- Paren_Count --
   -----------------

   function Paren_Count (N : Node_Id) return Nat is
      C : Nat := 0;

   begin
      pragma Assert (N <= Nodes.Last);

      if Nodes.Table (N).Pflag1 then
         C := C + 1;
      end if;

      if Nodes.Table (N).Pflag2 then
         C := C + 2;
      end if;

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

   ------------
   -- Parent --
   ------------

   function Parent (N : Node_Id) return Node_Id is
   begin
      if Is_List_Member (N) then
         return Parent (List_Containing (N));
      else
         return Node_Id (Nodes.Table (N).Link);
      end if;
   end Parent;

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
      Nodes.Table (NewN).Comes_From_Source :=
        Nodes.Table (OldN).Comes_From_Source;
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

      --  We now set the parent of the new node to be the same as the
      --  parent of the source. Almost always this parent will be
      --  replaced by a new value when the relocated node is reattached
      --  to the tree, but by doing it now, we ensure that this node is
      --  not even temporarily disconnected from the tree. Note that this
      --  does not happen free, because in the list case, the parent does
      --  not get set.

      Set_Parent (New_Node, Parent (Source));

      --  If the node being relocated was a rewriting of some original
      --  node, then the relocated node has the same original node.

      if Orig_Nodes.Table (Source) /= Source then
         Orig_Nodes.Table (New_Node) := Orig_Nodes.Table (Source);
      end if;

      return New_Node;
   end Relocate_Node;

   -------------
   -- Replace --
   -------------

   procedure Replace (Old_Node, New_Node : Node_Id) is
      Old_Post : constant Boolean := Nodes.Table (Old_Node).Error_Posted;
      Old_HasA : constant Boolean := Nodes.Table (Old_Node).Has_Aspects;
      Old_CFS  : constant Boolean := Nodes.Table (Old_Node).Comes_From_Source;

   begin
      pragma Assert
        (not Has_Extension (Old_Node)
          and not Has_Extension (New_Node)
          and not Nodes.Table (New_Node).In_List);

      --  Do copy, preserving link and in list status and required flags

      Copy_Node (Source => New_Node, Destination => Old_Node);
      Nodes.Table (Old_Node).Comes_From_Source := Old_CFS;
      Nodes.Table (Old_Node).Error_Posted      := Old_Post;
      Nodes.Table (Old_Node).Has_Aspects       := Old_HasA;

      --  Fix parents of substituted node, since it has changed identity

      Fix_Parents (Ref_Node => New_Node, Fix_Node => Old_Node);

      --  Since we are doing a replace, we assume that the original node
      --  is intended to become the new replaced node. The call would be
      --  to Rewrite if there were an intention to save the original node.

      Orig_Nodes.Table (Old_Node) := Old_Node;

      --  Invoke the reporting procedure (if available)

      if Reporting_Proc /= null then
         Reporting_Proc.all (Target => Old_Node, Source => New_Node);
      end if;
   end Replace;

   -------------
   -- Rewrite --
   -------------

   procedure Rewrite (Old_Node, New_Node : Node_Id) is
      Old_Error_P : constant Boolean  := Nodes.Table (Old_Node).Error_Posted;
      --  This field is always preserved in the new node

      Old_Has_Aspects : constant Boolean := Nodes.Table (Old_Node).Has_Aspects;
      --  This field is always preserved in the new node

      Old_Paren_Count     : Nat;
      Old_Must_Not_Freeze : Boolean;
      --  These fields are preserved in the new node only if the new node
      --  and the old node are both subexpression nodes.

      --  Note: it is a violation of abstraction levels for Must_Not_Freeze
      --  to be referenced like this. ???

      Sav_Node : Node_Id;

   begin
      pragma Assert
        (not Has_Extension (Old_Node)
          and not Has_Extension (New_Node)
          and not Nodes.Table (New_Node).In_List);
      pragma Debug (Rewrite_Debugging_Output (Old_Node, New_Node));

      if Nkind (Old_Node) in N_Subexpr then
         Old_Paren_Count     := Paren_Count (Old_Node);
         Old_Must_Not_Freeze := Must_Not_Freeze (Old_Node);
      else
         Old_Paren_Count     := 0;
         Old_Must_Not_Freeze := False;
      end if;

      --  Allocate a new node, to be used to preserve the original contents
      --  of the Old_Node, for possible later retrival by Original_Node and
      --  make an entry in the Orig_Nodes table. This is only done if we have
      --  not already rewritten the node, as indicated by an Orig_Nodes entry
      --  that does not reference the Old_Node.

      if Orig_Nodes.Table (Old_Node) = Old_Node then
         Sav_Node := New_Copy (Old_Node);
         Orig_Nodes.Table (Sav_Node) := Sav_Node;
         Orig_Nodes.Table (Old_Node) := Sav_Node;

         --  Both the old and new copies of the node will share the same list
         --  of aspect specifications if aspect specifications are present.

         if Old_Has_Aspects then
            Set_Aspect_Specifications
              (Sav_Node, Aspect_Specifications (Old_Node));
         end if;
      end if;

      --  Copy substitute node into place, preserving old fields as required

      Copy_Node (Source => New_Node, Destination => Old_Node);
      Nodes.Table (Old_Node).Error_Posted := Old_Error_P;
      Nodes.Table (Old_Node).Has_Aspects  := Old_Has_Aspects;

      if Nkind (New_Node) in N_Subexpr then
         Set_Paren_Count     (Old_Node, Old_Paren_Count);
         Set_Must_Not_Freeze (Old_Node, Old_Must_Not_Freeze);
      end if;

      Fix_Parents (Ref_Node => New_Node, Fix_Node => Old_Node);

      --  Invoke the reporting procedure (if available)

      if Reporting_Proc /= null then
         Reporting_Proc.all (Target => Old_Node, Source => New_Node);
      end if;
   end Rewrite;

   -------------------------
   -- Rewrite_Breakpoint --
   -------------------------

   procedure rr is
   begin
      Write_Str ("Watched node ");
      Write_Int (Int (Watch_Node));
      Write_Str (" rewritten");
      Write_Eol;
   end rr;

   ------------------------------
   -- Rewrite_Debugging_Output --
   ------------------------------

   procedure rrd (Old_Node, New_Node : Node_Id) is
      Node_Is_Watched : constant Boolean := Old_Node = Watch_Node;

   begin
      if Debug_Flag_N or else Node_Is_Watched then
         Node_Debug_Output ("Rewrite", Old_Node);
         Node_Debug_Output ("into",    New_Node);

         if Node_Is_Watched then
            Rewrite_Breakpoint;
         end if;
      end if;
   end rrd;

   ------------------
   -- Set_Analyzed --
   ------------------

   procedure Set_Analyzed (N : Node_Id; Val : Boolean := True) is
   begin
      Nodes.Table (N).Analyzed := Val;
   end Set_Analyzed;

   ---------------------------
   -- Set_Comes_From_Source --
   ---------------------------

   procedure Set_Comes_From_Source (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (N <= Nodes.Last);
      Nodes.Table (N).Comes_From_Source := Val;
   end Set_Comes_From_Source;

   -----------------------------------
   -- Set_Comes_From_Source_Default --
   -----------------------------------

   procedure Set_Comes_From_Source_Default (Default : Boolean) is
   begin
      Default_Node.Comes_From_Source := Default;
   end Set_Comes_From_Source_Default;

   ---------------
   -- Set_Ekind --
   ---------------

   procedure Set_Ekind (E : Entity_Id; Val : Entity_Kind) is
   begin
      pragma Assert (Nkind (E) in N_Entity);
      Nodes.Table (E + 1).Nkind := E_To_N (Val);
   end Set_Ekind;

   ----------------------
   -- Set_Error_Posted --
   ----------------------

   procedure Set_Error_Posted (N : Node_Id; Val : Boolean := True) is
   begin
      Nodes.Table (N).Error_Posted := Val;
   end Set_Error_Posted;

   ---------------------
   -- Set_Has_Aspects --
   ---------------------

   procedure Set_Has_Aspects (N : Node_Id; Val : Boolean := True) is
   begin
      pragma Assert (N <= Nodes.Last);
      Nodes.Table (N).Has_Aspects := Val;
   end Set_Has_Aspects;

   -----------------------
   -- Set_Original_Node --
   -----------------------

   procedure Set_Original_Node (N : Node_Id; Val : Node_Id) is
   begin
      Orig_Nodes.Table (N) := Val;
   end Set_Original_Node;

   ---------------------
   -- Set_Paren_Count --
   ---------------------

   procedure Set_Paren_Count (N : Node_Id; Val : Nat) is
   begin
      pragma Assert (Nkind (N) in N_Subexpr);

      --  Value of 0,1,2 stored as is

      if Val <= 2 then
         Nodes.Table (N).Pflag1 := (Val mod 2 /= 0);
         Nodes.Table (N).Pflag2 := (Val = 2);

      --  Value of 3 or greater stores 3 in node and makes table entry

      else
         Nodes.Table (N).Pflag1 := True;
         Nodes.Table (N).Pflag2 := True;

         for J in Paren_Counts.First .. Paren_Counts.Last loop
            if N = Paren_Counts.Table (J).Nod then
               Paren_Counts.Table (J).Count := Val;
               return;
            end if;
         end loop;

         Paren_Counts.Append ((Nod => N, Count => Val));
      end if;
   end Set_Paren_Count;

   ----------------
   -- Set_Parent --
   ----------------

   procedure Set_Parent (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (not Nodes.Table (N).In_List);
      Nodes.Table (N).Link := Union_Id (Val);
   end Set_Parent;

   --------------
   -- Set_Sloc --
   --------------

   procedure Set_Sloc (N : Node_Id; Val : Source_Ptr) is
   begin
      Nodes.Table (N).Sloc := Val;
   end Set_Sloc;

   ----------
   -- Sloc --
   ----------

   function Sloc (N : Node_Id) return Source_Ptr is
   begin
      return Nodes.Table (N).Sloc;
   end Sloc;

   -------------------
   -- Traverse_Func --
   -------------------

   function Traverse_Func (Node : Node_Id) return Traverse_Final_Result is

      function Traverse_Field
        (Nod : Node_Id;
         Fld : Union_Id;
         FN  : Field_Num) return Traverse_Final_Result;
      --  Fld is one of the fields of Nod. If the field points to syntactic
      --  node or list, then this node or list is traversed, and the result is
      --  the result of this traversal. Otherwise a value of True is returned
      --  with no processing. FN is the number of the field (1 .. 5).

      --------------------
      -- Traverse_Field --
      --------------------

      function Traverse_Field
        (Nod : Node_Id;
         Fld : Union_Id;
         FN  : Field_Num) return Traverse_Final_Result
      is
      begin
         if Fld = Union_Id (Empty) then
            return OK;

         --  Descendent is a node

         elsif Fld in Node_Range then

            --  Traverse descendent that is syntactic subtree node

            if Is_Syntactic_Field (Nkind (Nod), FN) then
               return Traverse_Func (Node_Id (Fld));

            --  Node that is not a syntactic subtree

            else
               return OK;
            end if;

         --  Descendent is a list

         elsif Fld in List_Range then

            --  Traverse descendent that is a syntactic subtree list

            if Is_Syntactic_Field (Nkind (Nod), FN) then
               declare
                  Elmt : Node_Id := First (List_Id (Fld));

               begin
                  while Present (Elmt) loop
                     if Traverse_Func (Elmt) = Abandon then
                        return Abandon;
                     else
                        Next (Elmt);
                     end if;
                  end loop;

                  return OK;
               end;

            --  List that is not a syntactic subtree

            else
               return OK;
            end if;

         --  Field was not a node or a list

         else
            return OK;
         end if;
      end Traverse_Field;

      Cur_Node : Node_Id := Node;

   --  Start of processing for Traverse_Func

   begin
      --  We walk Field2 last, and if it is a node, we eliminate the tail
      --  recursion by jumping back to this label. This is because Field2 is
      --  where the Left_Opnd field of N_Op_Concat is stored, and in practice
      --  concatenations are sometimes deeply nested, as in X1&X2&...&XN. This
      --  trick prevents us from running out of memory in that case. We don't
      --  bother eliminating the tail recursion if Field2 is a list.

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

      if Traverse_Field (Cur_Node, Field1 (Cur_Node), 1) = Abandon
           or else  --  skip Field2 here
         Traverse_Field (Cur_Node, Field3 (Cur_Node), 3) = Abandon
           or else
         Traverse_Field (Cur_Node, Field4 (Cur_Node), 4) = Abandon
           or else
         Traverse_Field (Cur_Node, Field5 (Cur_Node), 5) = Abandon
      then
         return Abandon;
      end if;

      if Field2 (Cur_Node) not in Node_Range then
         return Traverse_Field (Cur_Node, Field2 (Cur_Node), 2);

      elsif Is_Syntactic_Field (Nkind (Cur_Node), 2)
        and then Field2 (Cur_Node) /= Empty_List_Or_Node
      then
         --  Here is the tail recursion step, we reset Cur_Node and jump back
         --  to the start of the procedure, which has the same semantic effect
         --  as a call.

         Cur_Node := Node_Id (Field2 (Cur_Node));
         goto Tail_Recurse;
      end if;

      return OK;
   end Traverse_Func;

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

   ---------------
   -- Tree_Read --
   ---------------

   procedure Tree_Read is
   begin
      Tree_Read_Int (Node_Count);
      Nodes.Tree_Read;
      Flags.Tree_Read;
      Orig_Nodes.Tree_Read;
      Paren_Counts.Tree_Read;
   end Tree_Read;

   ----------------
   -- Tree_Write --
   ----------------

   procedure Tree_Write is
   begin
      Tree_Write_Int (Node_Count);
      Nodes.Tree_Write;
      Flags.Tree_Write;
      Orig_Nodes.Tree_Write;
      Paren_Counts.Tree_Write;
   end Tree_Write;

   ------------------------------
   -- Unchecked Access Package --
   ------------------------------

   package body Unchecked_Access is

      function Field1 (N : Node_Id) return Union_Id is
      begin
         pragma Assert (N <= Nodes.Last);
         return Nodes.Table (N).Field1;
      end Field1;

      function Field2 (N : Node_Id) return Union_Id is
      begin
         pragma Assert (N <= Nodes.Last);
         return Nodes.Table (N).Field2;
      end Field2;

      function Field3 (N : Node_Id) return Union_Id is
      begin
         pragma Assert (N <= Nodes.Last);
         return Nodes.Table (N).Field3;
      end Field3;

      function Field4 (N : Node_Id) return Union_Id is
      begin
         pragma Assert (N <= Nodes.Last);
         return Nodes.Table (N).Field4;
      end Field4;

      function Field5 (N : Node_Id) return Union_Id is
      begin
         pragma Assert (N <= Nodes.Last);
         return Nodes.Table (N).Field5;
      end Field5;

      function Field6 (N : Node_Id) return Union_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 1).Field6;
      end Field6;

      function Field7 (N : Node_Id) return Union_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 1).Field7;
      end Field7;

      function Field8 (N : Node_Id) return Union_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 1).Field8;
      end Field8;

      function Field9 (N : Node_Id) return Union_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 1).Field9;
      end Field9;

      function Field10 (N : Node_Id) return Union_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 1).Field10;
      end Field10;

      function Field11 (N : Node_Id) return Union_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 1).Field11;
      end Field11;

      function Field12 (N : Node_Id) return Union_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 1).Field12;
      end Field12;

      function Field13 (N : Node_Id) return Union_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 2).Field6;
      end Field13;

      function Field14 (N : Node_Id) return Union_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 2).Field7;
      end Field14;

      function Field15 (N : Node_Id) return Union_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 2).Field8;
      end Field15;

      function Field16 (N : Node_Id) return Union_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 2).Field9;
      end Field16;

      function Field17 (N : Node_Id) return Union_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 2).Field10;
      end Field17;

      function Field18 (N : Node_Id) return Union_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 2).Field11;
      end Field18;

      function Field19 (N : Node_Id) return Union_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 3).Field6;
      end Field19;

      function Field20 (N : Node_Id) return Union_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 3).Field7;
      end Field20;

      function Field21 (N : Node_Id) return Union_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 3).Field8;
      end Field21;

      function Field22 (N : Node_Id) return Union_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 3).Field9;
      end Field22;

      function Field23 (N : Node_Id) return Union_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 3).Field10;
      end Field23;

      function Field24 (N : Node_Id) return Union_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 4).Field6;
      end Field24;

      function Field25 (N : Node_Id) return Union_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 4).Field7;
      end Field25;

      function Field26 (N : Node_Id) return Union_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 4).Field8;
      end Field26;

      function Field27 (N : Node_Id) return Union_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 4).Field9;
      end Field27;

      function Field28 (N : Node_Id) return Union_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 4).Field10;
      end Field28;

      function Field29 (N : Node_Id) return Union_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 4).Field11;
      end Field29;

      function Field30 (N : Node_Id) return Union_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 5).Field6;
      end Field30;

      function Field31 (N : Node_Id) return Union_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 5).Field7;
      end Field31;

      function Field32 (N : Node_Id) return Union_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 5).Field8;
      end Field32;

      function Field33 (N : Node_Id) return Union_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 5).Field9;
      end Field33;

      function Field34 (N : Node_Id) return Union_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 5).Field10;
      end Field34;

      function Field35 (N : Node_Id) return Union_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 5).Field11;
      end Field35;

      function Node1 (N : Node_Id) return Node_Id is
      begin
         pragma Assert (N <= Nodes.Last);
         return Node_Id (Nodes.Table (N).Field1);
      end Node1;

      function Node2 (N : Node_Id) return Node_Id is
      begin
         pragma Assert (N <= Nodes.Last);
         return Node_Id (Nodes.Table (N).Field2);
      end Node2;

      function Node3 (N : Node_Id) return Node_Id is
      begin
         pragma Assert (N <= Nodes.Last);
         return Node_Id (Nodes.Table (N).Field3);
      end Node3;

      function Node4 (N : Node_Id) return Node_Id is
      begin
         pragma Assert (N <= Nodes.Last);
         return Node_Id (Nodes.Table (N).Field4);
      end Node4;

      function Node5 (N : Node_Id) return Node_Id is
      begin
         pragma Assert (N <= Nodes.Last);
         return Node_Id (Nodes.Table (N).Field5);
      end Node5;

      function Node6 (N : Node_Id) return Node_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Node_Id (Nodes.Table (N + 1).Field6);
      end Node6;

      function Node7 (N : Node_Id) return Node_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Node_Id (Nodes.Table (N + 1).Field7);
      end Node7;

      function Node8 (N : Node_Id) return Node_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Node_Id (Nodes.Table (N + 1).Field8);
      end Node8;

      function Node9 (N : Node_Id) return Node_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Node_Id (Nodes.Table (N + 1).Field9);
      end Node9;

      function Node10 (N : Node_Id) return Node_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Node_Id (Nodes.Table (N + 1).Field10);
      end Node10;

      function Node11 (N : Node_Id) return Node_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Node_Id (Nodes.Table (N + 1).Field11);
      end Node11;

      function Node12 (N : Node_Id) return Node_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Node_Id (Nodes.Table (N + 1).Field12);
      end Node12;

      function Node13 (N : Node_Id) return Node_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Node_Id (Nodes.Table (N + 2).Field6);
      end Node13;

      function Node14 (N : Node_Id) return Node_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Node_Id (Nodes.Table (N + 2).Field7);
      end Node14;

      function Node15 (N : Node_Id) return Node_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Node_Id (Nodes.Table (N + 2).Field8);
      end Node15;

      function Node16 (N : Node_Id) return Node_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Node_Id (Nodes.Table (N + 2).Field9);
      end Node16;

      function Node17 (N : Node_Id) return Node_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Node_Id (Nodes.Table (N + 2).Field10);
      end Node17;

      function Node18 (N : Node_Id) return Node_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Node_Id (Nodes.Table (N + 2).Field11);
      end Node18;

      function Node19 (N : Node_Id) return Node_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Node_Id (Nodes.Table (N + 3).Field6);
      end Node19;

      function Node20 (N : Node_Id) return Node_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Node_Id (Nodes.Table (N + 3).Field7);
      end Node20;

      function Node21 (N : Node_Id) return Node_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Node_Id (Nodes.Table (N + 3).Field8);
      end Node21;

      function Node22 (N : Node_Id) return Node_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Node_Id (Nodes.Table (N + 3).Field9);
      end Node22;

      function Node23 (N : Node_Id) return Node_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Node_Id (Nodes.Table (N + 3).Field10);
      end Node23;

      function Node24 (N : Node_Id) return Node_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Node_Id (Nodes.Table (N + 4).Field6);
      end Node24;

      function Node25 (N : Node_Id) return Node_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Node_Id (Nodes.Table (N + 4).Field7);
      end Node25;

      function Node26 (N : Node_Id) return Node_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Node_Id (Nodes.Table (N + 4).Field8);
      end Node26;

      function Node27 (N : Node_Id) return Node_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Node_Id (Nodes.Table (N + 4).Field9);
      end Node27;

      function Node28 (N : Node_Id) return Node_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Node_Id (Nodes.Table (N + 4).Field10);
      end Node28;

      function Node29 (N : Node_Id) return Node_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Node_Id (Nodes.Table (N + 4).Field11);
      end Node29;

      function Node30 (N : Node_Id) return Node_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Node_Id (Nodes.Table (N + 5).Field6);
      end Node30;

      function Node31 (N : Node_Id) return Node_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Node_Id (Nodes.Table (N + 5).Field7);
      end Node31;

      function Node32 (N : Node_Id) return Node_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Node_Id (Nodes.Table (N + 5).Field8);
      end Node32;

      function Node33 (N : Node_Id) return Node_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Node_Id (Nodes.Table (N + 5).Field9);
      end Node33;

      function Node34 (N : Node_Id) return Node_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Node_Id (Nodes.Table (N + 5).Field10);
      end Node34;

      function Node35 (N : Node_Id) return Node_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Node_Id (Nodes.Table (N + 5).Field11);
      end Node35;

      function List1 (N : Node_Id) return List_Id is
      begin
         pragma Assert (N <= Nodes.Last);
         return List_Id (Nodes.Table (N).Field1);
      end List1;

      function List2 (N : Node_Id) return List_Id is
      begin
         pragma Assert (N <= Nodes.Last);
         return List_Id (Nodes.Table (N).Field2);
      end List2;

      function List3 (N : Node_Id) return List_Id is
      begin
         pragma Assert (N <= Nodes.Last);
         return List_Id (Nodes.Table (N).Field3);
      end List3;

      function List4 (N : Node_Id) return List_Id is
      begin
         pragma Assert (N <= Nodes.Last);
         return List_Id (Nodes.Table (N).Field4);
      end List4;

      function List5 (N : Node_Id) return List_Id is
      begin
         pragma Assert (N <= Nodes.Last);
         return List_Id (Nodes.Table (N).Field5);
      end List5;

      function List10 (N : Node_Id) return List_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return List_Id (Nodes.Table (N + 1).Field10);
      end List10;

      function List14 (N : Node_Id) return List_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return List_Id (Nodes.Table (N + 2).Field7);
      end List14;

      function List25 (N : Node_Id) return List_Id is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return List_Id (Nodes.Table (N + 4).Field7);
      end List25;

      function Elist1 (N : Node_Id) return Elist_Id is
         pragma Assert (N <= Nodes.Last);
         Value : constant Union_Id := Nodes.Table (N).Field1;
      begin
         if Value = 0 then
            return No_Elist;
         else
            return Elist_Id (Value);
         end if;
      end Elist1;

      function Elist2 (N : Node_Id) return Elist_Id is
         pragma Assert (N <= Nodes.Last);
         Value : constant Union_Id := Nodes.Table (N).Field2;
      begin
         if Value = 0 then
            return No_Elist;
         else
            return Elist_Id (Value);
         end if;
      end Elist2;

      function Elist3 (N : Node_Id) return Elist_Id is
         pragma Assert (N <= Nodes.Last);
         Value : constant Union_Id := Nodes.Table (N).Field3;
      begin
         if Value = 0 then
            return No_Elist;
         else
            return Elist_Id (Value);
         end if;
      end Elist3;

      function Elist4 (N : Node_Id) return Elist_Id is
         pragma Assert (N <= Nodes.Last);
         Value : constant Union_Id := Nodes.Table (N).Field4;
      begin
         if Value = 0 then
            return No_Elist;
         else
            return Elist_Id (Value);
         end if;
      end Elist4;

      function Elist5 (N : Node_Id) return Elist_Id is
         pragma Assert (N <= Nodes.Last);
         Value : constant Union_Id := Nodes.Table (N).Field5;
      begin
         if Value = 0 then
            return No_Elist;
         else
            return Elist_Id (Value);
         end if;
      end Elist5;

      function Elist8 (N : Node_Id) return Elist_Id is
         pragma Assert (Nkind (N) in N_Entity);
         Value : constant Union_Id := Nodes.Table (N + 1).Field8;
      begin
         if Value = 0 then
            return No_Elist;
         else
            return Elist_Id (Value);
         end if;
      end Elist8;

      function Elist9 (N : Node_Id) return Elist_Id is
         pragma Assert (Nkind (N) in N_Entity);
         Value : constant Union_Id := Nodes.Table (N + 1).Field9;
      begin
         if Value = 0 then
            return No_Elist;
         else
            return Elist_Id (Value);
         end if;
      end Elist9;

      function Elist10 (N : Node_Id) return Elist_Id is
         pragma Assert (Nkind (N) in N_Entity);
         Value : constant Union_Id := Nodes.Table (N + 1).Field10;
      begin
         if Value = 0 then
            return No_Elist;
         else
            return Elist_Id (Value);
         end if;
      end Elist10;

      function Elist13 (N : Node_Id) return Elist_Id is
         pragma Assert (Nkind (N) in N_Entity);
         Value : constant Union_Id := Nodes.Table (N + 2).Field6;
      begin
         if Value = 0 then
            return No_Elist;
         else
            return Elist_Id (Value);
         end if;
      end Elist13;

      function Elist15 (N : Node_Id) return Elist_Id is
         pragma Assert (Nkind (N) in N_Entity);
         Value : constant Union_Id := Nodes.Table (N + 2).Field8;
      begin
         if Value = 0 then
            return No_Elist;
         else
            return Elist_Id (Value);
         end if;
      end Elist15;

      function Elist16 (N : Node_Id) return Elist_Id is
         pragma Assert (Nkind (N) in N_Entity);
         Value : constant Union_Id := Nodes.Table (N + 2).Field9;
      begin
         if Value = 0 then
            return No_Elist;
         else
            return Elist_Id (Value);
         end if;
      end Elist16;

      function Elist18 (N : Node_Id) return Elist_Id is
         pragma Assert (Nkind (N) in N_Entity);
         Value : constant Union_Id := Nodes.Table (N + 2).Field11;
      begin
         if Value = 0 then
            return No_Elist;
         else
            return Elist_Id (Value);
         end if;
      end Elist18;

      function Elist21 (N : Node_Id) return Elist_Id is
         pragma Assert (Nkind (N) in N_Entity);
         Value : constant Union_Id := Nodes.Table (N + 3).Field8;
      begin
         if Value = 0 then
            return No_Elist;
         else
            return Elist_Id (Value);
         end if;
      end Elist21;

      function Elist23 (N : Node_Id) return Elist_Id is
         pragma Assert (Nkind (N) in N_Entity);
         Value : constant Union_Id := Nodes.Table (N + 3).Field10;
      begin
         if Value = 0 then
            return No_Elist;
         else
            return Elist_Id (Value);
         end if;
      end Elist23;

      function Elist24 (N : Node_Id) return Elist_Id is
         pragma Assert (Nkind (N) in N_Entity);
         Value : constant Union_Id := Nodes.Table (N + 4).Field6;
      begin
         if Value = 0 then
            return No_Elist;
         else
            return Elist_Id (Value);
         end if;
      end Elist24;

      function Elist25 (N : Node_Id) return Elist_Id is
         pragma Assert (Nkind (N) in N_Entity);
         Value : constant Union_Id := Nodes.Table (N + 4).Field7;
      begin
         if Value = 0 then
            return No_Elist;
         else
            return Elist_Id (Value);
         end if;
      end Elist25;

      function Elist26 (N : Node_Id) return Elist_Id is
         pragma Assert (Nkind (N) in N_Entity);
         Value : constant Union_Id := Nodes.Table (N + 4).Field8;
      begin
         if Value = 0 then
            return No_Elist;
         else
            return Elist_Id (Value);
         end if;
      end Elist26;

      function Name1 (N : Node_Id) return Name_Id is
      begin
         pragma Assert (N <= Nodes.Last);
         return Name_Id (Nodes.Table (N).Field1);
      end Name1;

      function Name2 (N : Node_Id) return Name_Id is
      begin
         pragma Assert (N <= Nodes.Last);
         return Name_Id (Nodes.Table (N).Field2);
      end Name2;

      function Str3 (N : Node_Id) return String_Id is
      begin
         pragma Assert (N <= Nodes.Last);
         return String_Id (Nodes.Table (N).Field3);
      end Str3;

      function Uint2 (N : Node_Id) return Uint is
         pragma Assert (N <= Nodes.Last);
         U : constant Union_Id := Nodes.Table (N).Field2;
      begin
         if U = 0 then
            return Uint_0;
         else
            return From_Union (U);
         end if;
      end Uint2;

      function Uint3 (N : Node_Id) return Uint is
         pragma Assert (N <= Nodes.Last);
         U : constant Union_Id := Nodes.Table (N).Field3;
      begin
         if U = 0 then
            return Uint_0;
         else
            return From_Union (U);
         end if;
      end Uint3;

      function Uint4 (N : Node_Id) return Uint is
         pragma Assert (N <= Nodes.Last);
         U : constant Union_Id := Nodes.Table (N).Field4;
      begin
         if U = 0 then
            return Uint_0;
         else
            return From_Union (U);
         end if;
      end Uint4;

      function Uint5 (N : Node_Id) return Uint is
         pragma Assert (N <= Nodes.Last);
         U : constant Union_Id := Nodes.Table (N).Field5;
      begin
         if U = 0 then
            return Uint_0;
         else
            return From_Union (U);
         end if;
      end Uint5;

      function Uint8 (N : Node_Id) return Uint is
         pragma Assert (Nkind (N) in N_Entity);
         U : constant Union_Id := Nodes.Table (N + 1).Field8;
      begin
         if U = 0 then
            return Uint_0;
         else
            return From_Union (U);
         end if;
      end Uint8;

      function Uint9 (N : Node_Id) return Uint is
         pragma Assert (Nkind (N) in N_Entity);
         U : constant Union_Id := Nodes.Table (N + 1).Field9;
      begin
         if U = 0 then
            return Uint_0;
         else
            return From_Union (U);
         end if;
      end Uint9;

      function Uint10 (N : Node_Id) return Uint is
         pragma Assert (Nkind (N) in N_Entity);
         U : constant Union_Id := Nodes.Table (N + 1).Field10;
      begin
         if U = 0 then
            return Uint_0;
         else
            return From_Union (U);
         end if;
      end Uint10;

      function Uint11 (N : Node_Id) return Uint is
         pragma Assert (Nkind (N) in N_Entity);
         U : constant Union_Id := Nodes.Table (N + 1).Field11;
      begin
         if U = 0 then
            return Uint_0;
         else
            return From_Union (U);
         end if;
      end Uint11;

      function Uint12 (N : Node_Id) return Uint is
         pragma Assert (Nkind (N) in N_Entity);
         U : constant Union_Id := Nodes.Table (N + 1).Field12;
      begin
         if U = 0 then
            return Uint_0;
         else
            return From_Union (U);
         end if;
      end Uint12;

      function Uint13 (N : Node_Id) return Uint is
         pragma Assert (Nkind (N) in N_Entity);
         U : constant Union_Id := Nodes.Table (N + 2).Field6;
      begin
         if U = 0 then
            return Uint_0;
         else
            return From_Union (U);
         end if;
      end Uint13;

      function Uint14 (N : Node_Id) return Uint is
         pragma Assert (Nkind (N) in N_Entity);
         U : constant Union_Id := Nodes.Table (N + 2).Field7;
      begin
         if U = 0 then
            return Uint_0;
         else
            return From_Union (U);
         end if;
      end Uint14;

      function Uint15 (N : Node_Id) return Uint is
         pragma Assert (Nkind (N) in N_Entity);
         U : constant Union_Id := Nodes.Table (N + 2).Field8;
      begin
         if U = 0 then
            return Uint_0;
         else
            return From_Union (U);
         end if;
      end Uint15;

      function Uint16 (N : Node_Id) return Uint is
         pragma Assert (Nkind (N) in N_Entity);
         U : constant Union_Id := Nodes.Table (N + 2).Field9;
      begin
         if U = 0 then
            return Uint_0;
         else
            return From_Union (U);
         end if;
      end Uint16;

      function Uint17 (N : Node_Id) return Uint is
         pragma Assert (Nkind (N) in N_Entity);
         U : constant Union_Id := Nodes.Table (N + 2).Field10;
      begin
         if U = 0 then
            return Uint_0;
         else
            return From_Union (U);
         end if;
      end Uint17;

      function Uint22 (N : Node_Id) return Uint is
         pragma Assert (Nkind (N) in N_Entity);
         U : constant Union_Id := Nodes.Table (N + 3).Field9;
      begin
         if U = 0 then
            return Uint_0;
         else
            return From_Union (U);
         end if;
      end Uint22;

      function Ureal3 (N : Node_Id) return Ureal is
      begin
         pragma Assert (N <= Nodes.Last);
         return From_Union (Nodes.Table (N).Field3);
      end Ureal3;

      function Ureal18 (N : Node_Id) return Ureal is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return From_Union (Nodes.Table (N + 2).Field11);
      end Ureal18;

      function Ureal21 (N : Node_Id) return Ureal is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return From_Union (Nodes.Table (N + 3).Field8);
      end Ureal21;

      function Flag0 (N : Node_Id) return Boolean is
      begin
         pragma Assert (N <= Nodes.Last);
         return Flags.Table (N).Flag0;
      end Flag0;

      function Flag1 (N : Node_Id) return Boolean is
      begin
         pragma Assert (N <= Nodes.Last);
         return Flags.Table (N).Flag1;
      end Flag1;

      function Flag2 (N : Node_Id) return Boolean is
      begin
         pragma Assert (N <= Nodes.Last);
         return Flags.Table (N).Flag2;
      end Flag2;

      function Flag3 (N : Node_Id) return Boolean is
      begin
         pragma Assert (N <= Nodes.Last);
         return Flags.Table (N).Flag3;
      end Flag3;

      function Flag4 (N : Node_Id) return Boolean is
      begin
         pragma Assert (N <= Nodes.Last);
         return Nodes.Table (N).Flag4;
      end Flag4;

      function Flag5 (N : Node_Id) return Boolean is
      begin
         pragma Assert (N <= Nodes.Last);
         return Nodes.Table (N).Flag5;
      end Flag5;

      function Flag6 (N : Node_Id) return Boolean is
      begin
         pragma Assert (N <= Nodes.Last);
         return Nodes.Table (N).Flag6;
      end Flag6;

      function Flag7 (N : Node_Id) return Boolean is
      begin
         pragma Assert (N <= Nodes.Last);
         return Nodes.Table (N).Flag7;
      end Flag7;

      function Flag8 (N : Node_Id) return Boolean is
      begin
         pragma Assert (N <= Nodes.Last);
         return Nodes.Table (N).Flag8;
      end Flag8;

      function Flag9 (N : Node_Id) return Boolean is
      begin
         pragma Assert (N <= Nodes.Last);
         return Nodes.Table (N).Flag9;
      end Flag9;

      function Flag10 (N : Node_Id) return Boolean is
      begin
         pragma Assert (N <= Nodes.Last);
         return Nodes.Table (N).Flag10;
      end Flag10;

      function Flag11 (N : Node_Id) return Boolean is
      begin
         pragma Assert (N <= Nodes.Last);
         return Nodes.Table (N).Flag11;
      end Flag11;

      function Flag12 (N : Node_Id) return Boolean is
      begin
         pragma Assert (N <= Nodes.Last);
         return Nodes.Table (N).Flag12;
      end Flag12;

      function Flag13 (N : Node_Id) return Boolean is
      begin
         pragma Assert (N <= Nodes.Last);
         return Nodes.Table (N).Flag13;
      end Flag13;

      function Flag14 (N : Node_Id) return Boolean is
      begin
         pragma Assert (N <= Nodes.Last);
         return Nodes.Table (N).Flag14;
      end Flag14;

      function Flag15 (N : Node_Id) return Boolean is
      begin
         pragma Assert (N <= Nodes.Last);
         return Nodes.Table (N).Flag15;
      end Flag15;

      function Flag16 (N : Node_Id) return Boolean is
      begin
         pragma Assert (N <= Nodes.Last);
         return Nodes.Table (N).Flag16;
      end Flag16;

      function Flag17 (N : Node_Id) return Boolean is
      begin
         pragma Assert (N <= Nodes.Last);
         return Nodes.Table (N).Flag17;
      end Flag17;

      function Flag18 (N : Node_Id) return Boolean is
      begin
         pragma Assert (N <= Nodes.Last);
         return Nodes.Table (N).Flag18;
      end Flag18;

      function Flag19 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 1).In_List;
      end Flag19;

      function Flag20 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 1).Has_Aspects;
      end Flag20;

      function Flag21 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 1).Rewrite_Ins;
      end Flag21;

      function Flag22 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 1).Analyzed;
      end Flag22;

      function Flag23 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 1).Comes_From_Source;
      end Flag23;

      function Flag24 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 1).Error_Posted;
      end Flag24;

      function Flag25 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 1).Flag4;
      end Flag25;

      function Flag26 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 1).Flag5;
      end Flag26;

      function Flag27 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 1).Flag6;
      end Flag27;

      function Flag28 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 1).Flag7;
      end Flag28;

      function Flag29 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 1).Flag8;
      end Flag29;

      function Flag30 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 1).Flag9;
      end Flag30;

      function Flag31 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 1).Flag10;
      end Flag31;

      function Flag32 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 1).Flag11;
      end Flag32;

      function Flag33 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 1).Flag12;
      end Flag33;

      function Flag34 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 1).Flag13;
      end Flag34;

      function Flag35 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 1).Flag14;
      end Flag35;

      function Flag36 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 1).Flag15;
      end Flag36;

      function Flag37 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 1).Flag16;
      end Flag37;

      function Flag38 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 1).Flag17;
      end Flag38;

      function Flag39 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 1).Flag18;
      end Flag39;

      function Flag40 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 2).In_List;
      end Flag40;

      function Flag41 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 2).Has_Aspects;
      end Flag41;

      function Flag42 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 2).Rewrite_Ins;
      end Flag42;

      function Flag43 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 2).Analyzed;
      end Flag43;

      function Flag44 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 2).Comes_From_Source;
      end Flag44;

      function Flag45 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 2).Error_Posted;
      end Flag45;

      function Flag46 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 2).Flag4;
      end Flag46;

      function Flag47 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 2).Flag5;
      end Flag47;

      function Flag48 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 2).Flag6;
      end Flag48;

      function Flag49 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 2).Flag7;
      end Flag49;

      function Flag50 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 2).Flag8;
      end Flag50;

      function Flag51 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 2).Flag9;
      end Flag51;

      function Flag52 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 2).Flag10;
      end Flag52;

      function Flag53 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 2).Flag11;
      end Flag53;

      function Flag54 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 2).Flag12;
      end Flag54;

      function Flag55 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 2).Flag13;
      end Flag55;

      function Flag56 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 2).Flag14;
      end Flag56;

      function Flag57 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 2).Flag15;
      end Flag57;

      function Flag58 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 2).Flag16;
      end Flag58;

      function Flag59 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 2).Flag17;
      end Flag59;

      function Flag60 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 2).Flag18;
      end Flag60;

      function Flag61 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 1).Pflag1;
      end Flag61;

      function Flag62 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 1).Pflag2;
      end Flag62;

      function Flag63 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 2).Pflag1;
      end Flag63;

      function Flag64 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 2).Pflag2;
      end Flag64;

      function Flag65 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Byte (Nodes.Table (N + 2).Nkind).Flag65;
      end Flag65;

      function Flag66 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Byte (Nodes.Table (N + 2).Nkind).Flag66;
      end Flag66;

      function Flag67 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Byte (Nodes.Table (N + 2).Nkind).Flag67;
      end Flag67;

      function Flag68 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Byte (Nodes.Table (N + 2).Nkind).Flag68;
      end Flag68;

      function Flag69 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Byte (Nodes.Table (N + 2).Nkind).Flag69;
      end Flag69;

      function Flag70 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Byte (Nodes.Table (N + 2).Nkind).Flag70;
      end Flag70;

      function Flag71 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Byte (Nodes.Table (N + 2).Nkind).Flag71;
      end Flag71;

      function Flag72 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Byte (Nodes.Table (N + 2).Nkind).Flag72;
      end Flag72;

      function Flag73 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word (Nodes.Table (N + 2).Field12).Flag73;
      end Flag73;

      function Flag74 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word (Nodes.Table (N + 2).Field12).Flag74;
      end Flag74;

      function Flag75 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word (Nodes.Table (N + 2).Field12).Flag75;
      end Flag75;

      function Flag76 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word (Nodes.Table (N + 2).Field12).Flag76;
      end Flag76;

      function Flag77 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word (Nodes.Table (N + 2).Field12).Flag77;
      end Flag77;

      function Flag78 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word (Nodes.Table (N + 2).Field12).Flag78;
      end Flag78;

      function Flag79 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word (Nodes.Table (N + 2).Field12).Flag79;
      end Flag79;

      function Flag80 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word (Nodes.Table (N + 2).Field12).Flag80;
      end Flag80;

      function Flag81 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word (Nodes.Table (N + 2).Field12).Flag81;
      end Flag81;

      function Flag82 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word (Nodes.Table (N + 2).Field12).Flag82;
      end Flag82;

      function Flag83 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word (Nodes.Table (N + 2).Field12).Flag83;
      end Flag83;

      function Flag84 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word (Nodes.Table (N + 2).Field12).Flag84;
      end Flag84;

      function Flag85 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word (Nodes.Table (N + 2).Field12).Flag85;
      end Flag85;

      function Flag86 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word (Nodes.Table (N + 2).Field12).Flag86;
      end Flag86;

      function Flag87 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word (Nodes.Table (N + 2).Field12).Flag87;
      end Flag87;

      function Flag88 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word (Nodes.Table (N + 2).Field12).Flag88;
      end Flag88;

      function Flag89 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word (Nodes.Table (N + 2).Field12).Flag89;
      end Flag89;

      function Flag90 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word (Nodes.Table (N + 2).Field12).Flag90;
      end Flag90;

      function Flag91 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word (Nodes.Table (N + 2).Field12).Flag91;
      end Flag91;

      function Flag92 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word (Nodes.Table (N + 2).Field12).Flag92;
      end Flag92;

      function Flag93 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word (Nodes.Table (N + 2).Field12).Flag93;
      end Flag93;

      function Flag94 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word (Nodes.Table (N + 2).Field12).Flag94;
      end Flag94;

      function Flag95 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word (Nodes.Table (N + 2).Field12).Flag95;
      end Flag95;

      function Flag96 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word (Nodes.Table (N + 2).Field12).Flag96;
      end Flag96;

      function Flag97 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word2 (Nodes.Table (N + 3).Field12).Flag97;
      end Flag97;

      function Flag98 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word2 (Nodes.Table (N + 3).Field12).Flag98;
      end Flag98;

      function Flag99 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word2 (Nodes.Table (N + 3).Field12).Flag99;
      end Flag99;

      function Flag100 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word2 (Nodes.Table (N + 3).Field12).Flag100;
      end Flag100;

      function Flag101 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word2 (Nodes.Table (N + 3).Field12).Flag101;
      end Flag101;

      function Flag102 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word2 (Nodes.Table (N + 3).Field12).Flag102;
      end Flag102;

      function Flag103 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word2 (Nodes.Table (N + 3).Field12).Flag103;
      end Flag103;

      function Flag104 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word2 (Nodes.Table (N + 3).Field12).Flag104;
      end Flag104;

      function Flag105 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word2 (Nodes.Table (N + 3).Field12).Flag105;
      end Flag105;

      function Flag106 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word2 (Nodes.Table (N + 3).Field12).Flag106;
      end Flag106;

      function Flag107 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word2 (Nodes.Table (N + 3).Field12).Flag107;
      end Flag107;

      function Flag108 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word2 (Nodes.Table (N + 3).Field12).Flag108;
      end Flag108;

      function Flag109 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word2 (Nodes.Table (N + 3).Field12).Flag109;
      end Flag109;

      function Flag110 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word2 (Nodes.Table (N + 3).Field12).Flag110;
      end Flag110;

      function Flag111 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word2 (Nodes.Table (N + 3).Field12).Flag111;
      end Flag111;

      function Flag112 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word2 (Nodes.Table (N + 3).Field12).Flag112;
      end Flag112;

      function Flag113 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word2 (Nodes.Table (N + 3).Field12).Flag113;
      end Flag113;

      function Flag114 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word2 (Nodes.Table (N + 3).Field12).Flag114;
      end Flag114;

      function Flag115 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word2 (Nodes.Table (N + 3).Field12).Flag115;
      end Flag115;

      function Flag116 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word2 (Nodes.Table (N + 3).Field12).Flag116;
      end Flag116;

      function Flag117 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word2 (Nodes.Table (N + 3).Field12).Flag117;
      end Flag117;

      function Flag118 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word2 (Nodes.Table (N + 3).Field12).Flag118;
      end Flag118;

      function Flag119 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word2 (Nodes.Table (N + 3).Field12).Flag119;
      end Flag119;

      function Flag120 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word2 (Nodes.Table (N + 3).Field12).Flag120;
      end Flag120;

      function Flag121 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word2 (Nodes.Table (N + 3).Field12).Flag121;
      end Flag121;

      function Flag122 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word2 (Nodes.Table (N + 3).Field12).Flag122;
      end Flag122;

      function Flag123 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word2 (Nodes.Table (N + 3).Field12).Flag123;
      end Flag123;

      function Flag124 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word2 (Nodes.Table (N + 3).Field12).Flag124;
      end Flag124;

      function Flag125 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word2 (Nodes.Table (N + 3).Field12).Flag125;
      end Flag125;

      function Flag126 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word2 (Nodes.Table (N + 3).Field12).Flag126;
      end Flag126;

      function Flag127 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word2 (Nodes.Table (N + 3).Field12).Flag127;
      end Flag127;

      function Flag128 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word2 (Nodes.Table (N + 3).Field12).Flag128;
      end Flag128;

      function Flag129 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 3).In_List;
      end Flag129;

      function Flag130 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 3).Has_Aspects;
      end Flag130;

      function Flag131 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 3).Rewrite_Ins;
      end Flag131;

      function Flag132 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 3).Analyzed;
      end Flag132;

      function Flag133 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 3).Comes_From_Source;
      end Flag133;

      function Flag134 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 3).Error_Posted;
      end Flag134;

      function Flag135 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 3).Flag4;
      end Flag135;

      function Flag136 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 3).Flag5;
      end Flag136;

      function Flag137 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 3).Flag6;
      end Flag137;

      function Flag138 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 3).Flag7;
      end Flag138;

      function Flag139 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 3).Flag8;
      end Flag139;

      function Flag140 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 3).Flag9;
      end Flag140;

      function Flag141 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 3).Flag10;
      end Flag141;

      function Flag142 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 3).Flag11;
      end Flag142;

      function Flag143 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 3).Flag12;
      end Flag143;

      function Flag144 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 3).Flag13;
      end Flag144;

      function Flag145 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 3).Flag14;
      end Flag145;

      function Flag146 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 3).Flag15;
      end Flag146;

      function Flag147 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 3).Flag16;
      end Flag147;

      function Flag148 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 3).Flag17;
      end Flag148;

      function Flag149 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 3).Flag18;
      end Flag149;

      function Flag150 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 3).Pflag1;
      end Flag150;

      function Flag151 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 3).Pflag2;
      end Flag151;

      function Flag152 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word3 (Nodes.Table (N + 3).Field11).Flag152;
      end Flag152;

      function Flag153 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word3 (Nodes.Table (N + 3).Field11).Flag153;
      end Flag153;

      function Flag154 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word3 (Nodes.Table (N + 3).Field11).Flag154;
      end Flag154;

      function Flag155 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word3 (Nodes.Table (N + 3).Field11).Flag155;
      end Flag155;

      function Flag156 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word3 (Nodes.Table (N + 3).Field11).Flag156;
      end Flag156;

      function Flag157 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word3 (Nodes.Table (N + 3).Field11).Flag157;
      end Flag157;

      function Flag158 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word3 (Nodes.Table (N + 3).Field11).Flag158;
      end Flag158;

      function Flag159 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word3 (Nodes.Table (N + 3).Field11).Flag159;
      end Flag159;

      function Flag160 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word3 (Nodes.Table (N + 3).Field11).Flag160;
      end Flag160;

      function Flag161 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word3 (Nodes.Table (N + 3).Field11).Flag161;
      end Flag161;

      function Flag162 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word3 (Nodes.Table (N + 3).Field11).Flag162;
      end Flag162;

      function Flag163 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word3 (Nodes.Table (N + 3).Field11).Flag163;
      end Flag163;

      function Flag164 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word3 (Nodes.Table (N + 3).Field11).Flag164;
      end Flag164;

      function Flag165 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word3 (Nodes.Table (N + 3).Field11).Flag165;
      end Flag165;

      function Flag166 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word3 (Nodes.Table (N + 3).Field11).Flag166;
      end Flag166;

      function Flag167 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word3 (Nodes.Table (N + 3).Field11).Flag167;
      end Flag167;

      function Flag168 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word3 (Nodes.Table (N + 3).Field11).Flag168;
      end Flag168;

      function Flag169 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word3 (Nodes.Table (N + 3).Field11).Flag169;
      end Flag169;

      function Flag170 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word3 (Nodes.Table (N + 3).Field11).Flag170;
      end Flag170;

      function Flag171 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word3 (Nodes.Table (N + 3).Field11).Flag171;
      end Flag171;

      function Flag172 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word3 (Nodes.Table (N + 3).Field11).Flag172;
      end Flag172;

      function Flag173 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word3 (Nodes.Table (N + 3).Field11).Flag173;
      end Flag173;

      function Flag174 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word3 (Nodes.Table (N + 3).Field11).Flag174;
      end Flag174;

      function Flag175 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word3 (Nodes.Table (N + 3).Field11).Flag175;
      end Flag175;

      function Flag176 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word3 (Nodes.Table (N + 3).Field11).Flag176;
      end Flag176;

      function Flag177 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word3 (Nodes.Table (N + 3).Field11).Flag177;
      end Flag177;

      function Flag178 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word3 (Nodes.Table (N + 3).Field11).Flag178;
      end Flag178;

      function Flag179 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word3 (Nodes.Table (N + 3).Field11).Flag179;
      end Flag179;

      function Flag180 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word3 (Nodes.Table (N + 3).Field11).Flag180;
      end Flag180;

      function Flag181 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word3 (Nodes.Table (N + 3).Field11).Flag181;
      end Flag181;

      function Flag182 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word3 (Nodes.Table (N + 3).Field11).Flag182;
      end Flag182;

      function Flag183 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word3 (Nodes.Table (N + 3).Field11).Flag183;
      end Flag183;

      function Flag184 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word4 (Nodes.Table (N + 4).Field12).Flag184;
      end Flag184;

      function Flag185 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word4 (Nodes.Table (N + 4).Field12).Flag185;
      end Flag185;

      function Flag186 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word4 (Nodes.Table (N + 4).Field12).Flag186;
      end Flag186;

      function Flag187 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word4 (Nodes.Table (N + 4).Field12).Flag187;
      end Flag187;

      function Flag188 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word4 (Nodes.Table (N + 4).Field12).Flag188;
      end Flag188;

      function Flag189 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word4 (Nodes.Table (N + 4).Field12).Flag189;
      end Flag189;

      function Flag190 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word4 (Nodes.Table (N + 4).Field12).Flag190;
      end Flag190;

      function Flag191 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word4 (Nodes.Table (N + 4).Field12).Flag191;
      end Flag191;

      function Flag192 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word4 (Nodes.Table (N + 4).Field12).Flag192;
      end Flag192;

      function Flag193 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word4 (Nodes.Table (N + 4).Field12).Flag193;
      end Flag193;

      function Flag194 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word4 (Nodes.Table (N + 4).Field12).Flag194;
      end Flag194;

      function Flag195 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word4 (Nodes.Table (N + 4).Field12).Flag195;
      end Flag195;

      function Flag196 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word4 (Nodes.Table (N + 4).Field12).Flag196;
      end Flag196;

      function Flag197 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word4 (Nodes.Table (N + 4).Field12).Flag197;
      end Flag197;

      function Flag198 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word4 (Nodes.Table (N + 4).Field12).Flag198;
      end Flag198;

      function Flag199 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word4 (Nodes.Table (N + 4).Field12).Flag199;
      end Flag199;

      function Flag200 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word4 (Nodes.Table (N + 4).Field12).Flag200;
      end Flag200;

      function Flag201 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word4 (Nodes.Table (N + 4).Field12).Flag201;
      end Flag201;

      function Flag202 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word4 (Nodes.Table (N + 4).Field12).Flag202;
      end Flag202;

      function Flag203 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word4 (Nodes.Table (N + 4).Field12).Flag203;
      end Flag203;

      function Flag204 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word4 (Nodes.Table (N + 4).Field12).Flag204;
      end Flag204;

      function Flag205 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word4 (Nodes.Table (N + 4).Field12).Flag205;
      end Flag205;

      function Flag206 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word4 (Nodes.Table (N + 4).Field12).Flag206;
      end Flag206;

      function Flag207 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word4 (Nodes.Table (N + 4).Field12).Flag207;
      end Flag207;

      function Flag208 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word4 (Nodes.Table (N + 4).Field12).Flag208;
      end Flag208;

      function Flag209 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word4 (Nodes.Table (N + 4).Field12).Flag209;
      end Flag209;

      function Flag210 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word4 (Nodes.Table (N + 4).Field12).Flag210;
      end Flag210;

      function Flag211 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word4 (Nodes.Table (N + 4).Field12).Flag211;
      end Flag211;

      function Flag212 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word4 (Nodes.Table (N + 4).Field12).Flag212;
      end Flag212;

      function Flag213 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word4 (Nodes.Table (N + 4).Field12).Flag213;
      end Flag213;

      function Flag214 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word4 (Nodes.Table (N + 4).Field12).Flag214;
      end Flag214;

      function Flag215 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word4 (Nodes.Table (N + 4).Field12).Flag215;
      end Flag215;

      function Flag216 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 4).In_List;
      end Flag216;

      function Flag217 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 4).Has_Aspects;
      end Flag217;

      function Flag218 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 4).Rewrite_Ins;
      end Flag218;

      function Flag219 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 4).Analyzed;
      end Flag219;

      function Flag220 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 4).Comes_From_Source;
      end Flag220;

      function Flag221 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 4).Error_Posted;
      end Flag221;

      function Flag222 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 4).Flag4;
      end Flag222;

      function Flag223 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 4).Flag5;
      end Flag223;

      function Flag224 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 4).Flag6;
      end Flag224;

      function Flag225 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 4).Flag7;
      end Flag225;

      function Flag226 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 4).Flag8;
      end Flag226;

      function Flag227 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 4).Flag9;
      end Flag227;

      function Flag228 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 4).Flag10;
      end Flag228;

      function Flag229 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 4).Flag11;
      end Flag229;

      function Flag230 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 4).Flag12;
      end Flag230;

      function Flag231 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 4).Flag13;
      end Flag231;

      function Flag232 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 4).Flag14;
      end Flag232;

      function Flag233 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 4).Flag15;
      end Flag233;

      function Flag234 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 4).Flag16;
      end Flag234;

      function Flag235 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 4).Flag17;
      end Flag235;

      function Flag236 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 4).Flag18;
      end Flag236;

      function Flag237 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 4).Pflag1;
      end Flag237;

      function Flag238 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 4).Pflag2;
      end Flag238;

      function Flag239 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Byte2 (Nodes.Table (N + 3).Nkind).Flag239;
      end Flag239;

      function Flag240 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Byte2 (Nodes.Table (N + 3).Nkind).Flag240;
      end Flag240;

      function Flag241 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Byte2 (Nodes.Table (N + 3).Nkind).Flag241;
      end Flag241;

      function Flag242 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Byte2 (Nodes.Table (N + 3).Nkind).Flag242;
      end Flag242;

      function Flag243 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Byte2 (Nodes.Table (N + 3).Nkind).Flag243;
      end Flag243;

      function Flag244 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Byte2 (Nodes.Table (N + 3).Nkind).Flag244;
      end Flag244;

      function Flag245 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Byte2 (Nodes.Table (N + 3).Nkind).Flag245;
      end Flag245;

      function Flag246 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Byte2 (Nodes.Table (N + 3).Nkind).Flag246;
      end Flag246;

      function Flag247 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Byte3 (Nodes.Table (N + 4).Nkind).Flag247;
      end Flag247;

      function Flag248 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Byte3 (Nodes.Table (N + 4).Nkind).Flag248;
      end Flag248;

      function Flag249 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Byte3 (Nodes.Table (N + 4).Nkind).Flag249;
      end Flag249;

      function Flag250 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Byte3 (Nodes.Table (N + 4).Nkind).Flag250;
      end Flag250;

      function Flag251 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Byte3 (Nodes.Table (N + 4).Nkind).Flag251;
      end Flag251;

      function Flag252 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Byte3 (Nodes.Table (N + 4).Nkind).Flag252;
      end Flag252;

      function Flag253 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Byte3 (Nodes.Table (N + 4).Nkind).Flag253;
      end Flag253;

      function Flag254 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Byte3 (Nodes.Table (N + 4).Nkind).Flag254;
      end Flag254;

      function Flag255 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word5 (Nodes.Table (N + 5).Field12).Flag255;
      end Flag255;

      function Flag256 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word5 (Nodes.Table (N + 5).Field12).Flag256;
      end Flag256;

      function Flag257 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word5 (Nodes.Table (N + 5).Field12).Flag257;
      end Flag257;

      function Flag258 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word5 (Nodes.Table (N + 5).Field12).Flag258;
      end Flag258;

      function Flag259 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word5 (Nodes.Table (N + 5).Field12).Flag259;
      end Flag259;

      function Flag260 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word5 (Nodes.Table (N + 5).Field12).Flag260;
      end Flag260;

      function Flag261 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word5 (Nodes.Table (N + 5).Field12).Flag261;
      end Flag261;

      function Flag262 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word5 (Nodes.Table (N + 5).Field12).Flag262;
      end Flag262;

      function Flag263 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word5 (Nodes.Table (N + 5).Field12).Flag263;
      end Flag263;

      function Flag264 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word5 (Nodes.Table (N + 5).Field12).Flag264;
      end Flag264;

      function Flag265 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word5 (Nodes.Table (N + 5).Field12).Flag265;
      end Flag265;

      function Flag266 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word5 (Nodes.Table (N + 5).Field12).Flag266;
      end Flag266;

      function Flag267 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word5 (Nodes.Table (N + 5).Field12).Flag267;
      end Flag267;

      function Flag268 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word5 (Nodes.Table (N + 5).Field12).Flag268;
      end Flag268;

      function Flag269 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word5 (Nodes.Table (N + 5).Field12).Flag269;
      end Flag269;

      function Flag270 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word5 (Nodes.Table (N + 5).Field12).Flag270;
      end Flag270;

      function Flag271 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word5 (Nodes.Table (N + 5).Field12).Flag271;
      end Flag271;

      function Flag272 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word5 (Nodes.Table (N + 5).Field12).Flag272;
      end Flag272;

      function Flag273 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word5 (Nodes.Table (N + 5).Field12).Flag273;
      end Flag273;

      function Flag274 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word5 (Nodes.Table (N + 5).Field12).Flag274;
      end Flag274;

      function Flag275 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word5 (Nodes.Table (N + 5).Field12).Flag275;
      end Flag275;

      function Flag276 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word5 (Nodes.Table (N + 5).Field12).Flag276;
      end Flag276;

      function Flag277 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word5 (Nodes.Table (N + 5).Field12).Flag277;
      end Flag277;

      function Flag278 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word5 (Nodes.Table (N + 5).Field12).Flag278;
      end Flag278;

      function Flag279 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word5 (Nodes.Table (N + 5).Field12).Flag279;
      end Flag279;

      function Flag280 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word5 (Nodes.Table (N + 5).Field12).Flag280;
      end Flag280;

      function Flag281 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word5 (Nodes.Table (N + 5).Field12).Flag281;
      end Flag281;

      function Flag282 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word5 (Nodes.Table (N + 5).Field12).Flag282;
      end Flag282;

      function Flag283 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word5 (Nodes.Table (N + 5).Field12).Flag283;
      end Flag283;

      function Flag284 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word5 (Nodes.Table (N + 5).Field12).Flag284;
      end Flag284;

      function Flag285 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word5 (Nodes.Table (N + 5).Field12).Flag285;
      end Flag285;

      function Flag286 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Word5 (Nodes.Table (N + 5).Field12).Flag286;
      end Flag286;

      function Flag287 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 5).In_List;
      end Flag287;

      function Flag288 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 5).Has_Aspects;
      end Flag288;

      function Flag289 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 5).Rewrite_Ins;
      end Flag289;

      function Flag290 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 5).Analyzed;
      end Flag290;

      function Flag291 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 5).Comes_From_Source;
      end Flag291;

      function Flag292 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 5).Error_Posted;
      end Flag292;

      function Flag293 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 5).Flag4;
      end Flag293;

      function Flag294 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 5).Flag5;
      end Flag294;

      function Flag295 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 5).Flag6;
      end Flag295;

      function Flag296 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 5).Flag7;
      end Flag296;

      function Flag297 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 5).Flag8;
      end Flag297;

      function Flag298 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 5).Flag9;
      end Flag298;

      function Flag299 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 5).Flag10;
      end Flag299;

      function Flag300 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 5).Flag11;
      end Flag300;

      function Flag301 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 5).Flag12;
      end Flag301;

      function Flag302 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 5).Flag13;
      end Flag302;

      function Flag303 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 5).Flag14;
      end Flag303;

      function Flag304 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 5).Flag15;
      end Flag304;

      function Flag305 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 5).Flag16;
      end Flag305;

      function Flag306 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 5).Flag17;
      end Flag306;

      function Flag307 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 5).Flag18;
      end Flag307;

      function Flag308 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 5).Pflag1;
      end Flag308;

      function Flag309 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return Nodes.Table (N + 5).Pflag2;
      end Flag309;

      function Flag310 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Byte4 (Nodes.Table (N + 5).Nkind).Flag310;
      end Flag310;

      function Flag311 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Byte4 (Nodes.Table (N + 5).Nkind).Flag311;
      end Flag311;

      function Flag312 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Byte4 (Nodes.Table (N + 5).Nkind).Flag312;
      end Flag312;

      function Flag313 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Byte4 (Nodes.Table (N + 5).Nkind).Flag313;
      end Flag313;

      function Flag314 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Byte4 (Nodes.Table (N + 5).Nkind).Flag314;
      end Flag314;

      function Flag315 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Byte4 (Nodes.Table (N + 5).Nkind).Flag315;
      end Flag315;

      function Flag316 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Byte4 (Nodes.Table (N + 5).Nkind).Flag316;
      end Flag316;

      function Flag317 (N : Node_Id) return Boolean is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         return To_Flag_Byte4 (Nodes.Table (N + 5).Nkind).Flag317;
      end Flag317;

      procedure Set_Nkind (N : Node_Id; Val : Node_Kind) is
      begin
         pragma Assert (N <= Nodes.Last);
         Nodes.Table (N).Nkind := Val;
      end Set_Nkind;

      procedure Set_Field1 (N : Node_Id; Val : Union_Id) is
      begin
         pragma Assert (N <= Nodes.Last);
         Nodes.Table (N).Field1 := Val;
      end Set_Field1;

      procedure Set_Field2 (N : Node_Id; Val : Union_Id) is
      begin
         pragma Assert (N <= Nodes.Last);
         Nodes.Table (N).Field2 := Val;
      end Set_Field2;

      procedure Set_Field3 (N : Node_Id; Val : Union_Id) is
      begin
         pragma Assert (N <= Nodes.Last);
         Nodes.Table (N).Field3 := Val;
      end Set_Field3;

      procedure Set_Field4 (N : Node_Id; Val : Union_Id) is
      begin
         pragma Assert (N <= Nodes.Last);
         Nodes.Table (N).Field4 := Val;
      end Set_Field4;

      procedure Set_Field5 (N : Node_Id; Val : Union_Id) is
      begin
         pragma Assert (N <= Nodes.Last);
         Nodes.Table (N).Field5 := Val;
      end Set_Field5;

      procedure Set_Field6 (N : Node_Id; Val : Union_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).Field6 := Val;
      end Set_Field6;

      procedure Set_Field7 (N : Node_Id; Val : Union_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).Field7 := Val;
      end Set_Field7;

      procedure Set_Field8 (N : Node_Id; Val : Union_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).Field8 := Val;
      end Set_Field8;

      procedure Set_Field9 (N : Node_Id; Val : Union_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).Field9 := Val;
      end Set_Field9;

      procedure Set_Field10 (N : Node_Id; Val : Union_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).Field10 := Val;
      end Set_Field10;

      procedure Set_Field11 (N : Node_Id; Val : Union_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).Field11 := Val;
      end Set_Field11;

      procedure Set_Field12 (N : Node_Id; Val : Union_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).Field12 := Val;
      end Set_Field12;

      procedure Set_Field13 (N : Node_Id; Val : Union_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).Field6 := Val;
      end Set_Field13;

      procedure Set_Field14 (N : Node_Id; Val : Union_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).Field7 := Val;
      end Set_Field14;

      procedure Set_Field15 (N : Node_Id; Val : Union_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).Field8 := Val;
      end Set_Field15;

      procedure Set_Field16 (N : Node_Id; Val : Union_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).Field9 := Val;
      end Set_Field16;

      procedure Set_Field17 (N : Node_Id; Val : Union_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).Field10 := Val;
      end Set_Field17;

      procedure Set_Field18 (N : Node_Id; Val : Union_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).Field11 := Val;
      end Set_Field18;

      procedure Set_Field19 (N : Node_Id; Val : Union_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 3).Field6 := Val;
      end Set_Field19;

      procedure Set_Field20 (N : Node_Id; Val : Union_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 3).Field7 := Val;
      end Set_Field20;

      procedure Set_Field21 (N : Node_Id; Val : Union_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 3).Field8 := Val;
      end Set_Field21;

      procedure Set_Field22 (N : Node_Id; Val : Union_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 3).Field9 := Val;
      end Set_Field22;

      procedure Set_Field23 (N : Node_Id; Val : Union_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 3).Field10 := Val;
      end Set_Field23;

      procedure Set_Field24 (N : Node_Id; Val : Union_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 4).Field6 := Val;
      end Set_Field24;

      procedure Set_Field25 (N : Node_Id; Val : Union_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 4).Field7 := Val;
      end Set_Field25;

      procedure Set_Field26 (N : Node_Id; Val : Union_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 4).Field8 := Val;
      end Set_Field26;

      procedure Set_Field27 (N : Node_Id; Val : Union_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 4).Field9 := Val;
      end Set_Field27;

      procedure Set_Field28 (N : Node_Id; Val : Union_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 4).Field10 := Val;
      end Set_Field28;

      procedure Set_Field29 (N : Node_Id; Val : Union_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 4).Field11 := Val;
      end Set_Field29;

      procedure Set_Field30 (N : Node_Id; Val : Union_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 5).Field6 := Val;
      end Set_Field30;

      procedure Set_Field31 (N : Node_Id; Val : Union_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 5).Field7 := Val;
      end Set_Field31;

      procedure Set_Field32 (N : Node_Id; Val : Union_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 5).Field8 := Val;
      end Set_Field32;

      procedure Set_Field33 (N : Node_Id; Val : Union_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 5).Field9 := Val;
      end Set_Field33;

      procedure Set_Field34 (N : Node_Id; Val : Union_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 5).Field10 := Val;
      end Set_Field34;

      procedure Set_Field35 (N : Node_Id; Val : Union_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 5).Field11 := Val;
      end Set_Field35;

      procedure Set_Node1 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Assert (N <= Nodes.Last);
         Nodes.Table (N).Field1 := Union_Id (Val);
      end Set_Node1;

      procedure Set_Node2 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Assert (N <= Nodes.Last);
         Nodes.Table (N).Field2 := Union_Id (Val);
      end Set_Node2;

      procedure Set_Node3 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Assert (N <= Nodes.Last);
         Nodes.Table (N).Field3 := Union_Id (Val);
      end Set_Node3;

      procedure Set_Node4 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Assert (N <= Nodes.Last);
         Nodes.Table (N).Field4 := Union_Id (Val);
      end Set_Node4;

      procedure Set_Node5 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Assert (N <= Nodes.Last);
         Nodes.Table (N).Field5 := Union_Id (Val);
      end Set_Node5;

      procedure Set_Node6 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).Field6 := Union_Id (Val);
      end Set_Node6;

      procedure Set_Node7 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).Field7 := Union_Id (Val);
      end Set_Node7;

      procedure Set_Node8 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).Field8 := Union_Id (Val);
      end Set_Node8;

      procedure Set_Node9 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).Field9 := Union_Id (Val);
      end Set_Node9;

      procedure Set_Node10 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).Field10 := Union_Id (Val);
      end Set_Node10;

      procedure Set_Node11 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).Field11 := Union_Id (Val);
      end Set_Node11;

      procedure Set_Node12 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).Field12 := Union_Id (Val);
      end Set_Node12;

      procedure Set_Node13 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).Field6 := Union_Id (Val);
      end Set_Node13;

      procedure Set_Node14 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).Field7 := Union_Id (Val);
      end Set_Node14;

      procedure Set_Node15 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).Field8 := Union_Id (Val);
      end Set_Node15;

      procedure Set_Node16 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).Field9 := Union_Id (Val);
      end Set_Node16;

      procedure Set_Node17 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).Field10 := Union_Id (Val);
      end Set_Node17;

      procedure Set_Node18 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).Field11 := Union_Id (Val);
      end Set_Node18;

      procedure Set_Node19 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 3).Field6 := Union_Id (Val);
      end Set_Node19;

      procedure Set_Node20 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 3).Field7 := Union_Id (Val);
      end Set_Node20;

      procedure Set_Node21 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 3).Field8 := Union_Id (Val);
      end Set_Node21;

      procedure Set_Node22 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 3).Field9 := Union_Id (Val);
      end Set_Node22;

      procedure Set_Node23 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 3).Field10 := Union_Id (Val);
      end Set_Node23;

      procedure Set_Node24 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 4).Field6 := Union_Id (Val);
      end Set_Node24;

      procedure Set_Node25 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 4).Field7 := Union_Id (Val);
      end Set_Node25;

      procedure Set_Node26 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 4).Field8 := Union_Id (Val);
      end Set_Node26;

      procedure Set_Node27 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 4).Field9 := Union_Id (Val);
      end Set_Node27;

      procedure Set_Node28 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 4).Field10 := Union_Id (Val);
      end Set_Node28;

      procedure Set_Node29 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 4).Field11 := Union_Id (Val);
      end Set_Node29;

      procedure Set_Node30 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 5).Field6 := Union_Id (Val);
      end Set_Node30;

      procedure Set_Node31 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 5).Field7 := Union_Id (Val);
      end Set_Node31;

      procedure Set_Node32 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 5).Field8 := Union_Id (Val);
      end Set_Node32;

      procedure Set_Node33 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 5).Field9 := Union_Id (Val);
      end Set_Node33;

      procedure Set_Node34 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 5).Field10 := Union_Id (Val);
      end Set_Node34;

      procedure Set_Node35 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 5).Field11 := Union_Id (Val);
      end Set_Node35;

      procedure Set_List1 (N : Node_Id; Val : List_Id) is
      begin
         pragma Assert (N <= Nodes.Last);
         Nodes.Table (N).Field1 := Union_Id (Val);
      end Set_List1;

      procedure Set_List2 (N : Node_Id; Val : List_Id) is
      begin
         pragma Assert (N <= Nodes.Last);
         Nodes.Table (N).Field2 := Union_Id (Val);
      end Set_List2;

      procedure Set_List3 (N : Node_Id; Val : List_Id) is
      begin
         pragma Assert (N <= Nodes.Last);
         Nodes.Table (N).Field3 := Union_Id (Val);
      end Set_List3;

      procedure Set_List4 (N : Node_Id; Val : List_Id) is
      begin
         pragma Assert (N <= Nodes.Last);
         Nodes.Table (N).Field4 := Union_Id (Val);
      end Set_List4;

      procedure Set_List5 (N : Node_Id; Val : List_Id) is
      begin
         pragma Assert (N <= Nodes.Last);
         Nodes.Table (N).Field5 := Union_Id (Val);
      end Set_List5;

      procedure Set_List10 (N : Node_Id; Val : List_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).Field10 := Union_Id (Val);
      end Set_List10;

      procedure Set_List14 (N : Node_Id; Val : List_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).Field7 := Union_Id (Val);
      end Set_List14;

      procedure Set_List25 (N : Node_Id; Val : List_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 4).Field7 := Union_Id (Val);
      end Set_List25;

      procedure Set_Elist1 (N : Node_Id; Val : Elist_Id) is
      begin
         Nodes.Table (N).Field1 := Union_Id (Val);
      end Set_Elist1;

      procedure Set_Elist2 (N : Node_Id; Val : Elist_Id) is
      begin
         Nodes.Table (N).Field2 := Union_Id (Val);
      end Set_Elist2;

      procedure Set_Elist3 (N : Node_Id; Val : Elist_Id) is
      begin
         Nodes.Table (N).Field3 := Union_Id (Val);
      end Set_Elist3;

      procedure Set_Elist4 (N : Node_Id; Val : Elist_Id) is
      begin
         Nodes.Table (N).Field4 := Union_Id (Val);
      end Set_Elist4;

      procedure Set_Elist5 (N : Node_Id; Val : Elist_Id) is
      begin
         Nodes.Table (N).Field5 := Union_Id (Val);
      end Set_Elist5;

      procedure Set_Elist8 (N : Node_Id; Val : Elist_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).Field8 := Union_Id (Val);
      end Set_Elist8;

      procedure Set_Elist9 (N : Node_Id; Val : Elist_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).Field9 := Union_Id (Val);
      end Set_Elist9;

      procedure Set_Elist10 (N : Node_Id; Val : Elist_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).Field10 := Union_Id (Val);
      end Set_Elist10;

      procedure Set_Elist13 (N : Node_Id; Val : Elist_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).Field6 := Union_Id (Val);
      end Set_Elist13;

      procedure Set_Elist15 (N : Node_Id; Val : Elist_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).Field8 := Union_Id (Val);
      end Set_Elist15;

      procedure Set_Elist16 (N : Node_Id; Val : Elist_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).Field9 := Union_Id (Val);
      end Set_Elist16;

      procedure Set_Elist18 (N : Node_Id; Val : Elist_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).Field11 := Union_Id (Val);
      end Set_Elist18;

      procedure Set_Elist21 (N : Node_Id; Val : Elist_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 3).Field8 := Union_Id (Val);
      end Set_Elist21;

      procedure Set_Elist23 (N : Node_Id; Val : Elist_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 3).Field10 := Union_Id (Val);
      end Set_Elist23;

      procedure Set_Elist24 (N : Node_Id; Val : Elist_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 4).Field6 := Union_Id (Val);
      end Set_Elist24;

      procedure Set_Elist25 (N : Node_Id; Val : Elist_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 4).Field7 := Union_Id (Val);
      end Set_Elist25;

      procedure Set_Elist26 (N : Node_Id; Val : Elist_Id) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 4).Field8 := Union_Id (Val);
      end Set_Elist26;

      procedure Set_Name1 (N : Node_Id; Val : Name_Id) is
      begin
         pragma Assert (N <= Nodes.Last);
         Nodes.Table (N).Field1 := Union_Id (Val);
      end Set_Name1;

      procedure Set_Name2 (N : Node_Id; Val : Name_Id) is
      begin
         pragma Assert (N <= Nodes.Last);
         Nodes.Table (N).Field2 := Union_Id (Val);
      end Set_Name2;

      procedure Set_Str3 (N : Node_Id; Val : String_Id) is
      begin
         pragma Assert (N <= Nodes.Last);
         Nodes.Table (N).Field3 := Union_Id (Val);
      end Set_Str3;

      procedure Set_Uint2 (N : Node_Id; Val : Uint) is
      begin
         pragma Assert (N <= Nodes.Last);
         Nodes.Table (N).Field2 := To_Union (Val);
      end Set_Uint2;

      procedure Set_Uint3 (N : Node_Id; Val : Uint) is
      begin
         pragma Assert (N <= Nodes.Last);
         Nodes.Table (N).Field3 := To_Union (Val);
      end Set_Uint3;

      procedure Set_Uint4 (N : Node_Id; Val : Uint) is
      begin
         pragma Assert (N <= Nodes.Last);
         Nodes.Table (N).Field4 := To_Union (Val);
      end Set_Uint4;

      procedure Set_Uint5 (N : Node_Id; Val : Uint) is
      begin
         pragma Assert (N <= Nodes.Last);
         Nodes.Table (N).Field5 := To_Union (Val);
      end Set_Uint5;

      procedure Set_Uint8 (N : Node_Id; Val : Uint) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).Field8 := To_Union (Val);
      end Set_Uint8;

      procedure Set_Uint9 (N : Node_Id; Val : Uint) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).Field9 := To_Union (Val);
      end Set_Uint9;

      procedure Set_Uint10 (N : Node_Id; Val : Uint) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).Field10 := To_Union (Val);
      end Set_Uint10;

      procedure Set_Uint11 (N : Node_Id; Val : Uint) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).Field11 := To_Union (Val);
      end Set_Uint11;

      procedure Set_Uint12 (N : Node_Id; Val : Uint) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).Field12 := To_Union (Val);
      end Set_Uint12;

      procedure Set_Uint13 (N : Node_Id; Val : Uint) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).Field6 := To_Union (Val);
      end Set_Uint13;

      procedure Set_Uint14 (N : Node_Id; Val : Uint) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).Field7 := To_Union (Val);
      end Set_Uint14;

      procedure Set_Uint15 (N : Node_Id; Val : Uint) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).Field8 := To_Union (Val);
      end Set_Uint15;

      procedure Set_Uint16 (N : Node_Id; Val : Uint) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).Field9 := To_Union (Val);
      end Set_Uint16;

      procedure Set_Uint17 (N : Node_Id; Val : Uint) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).Field10 := To_Union (Val);
      end Set_Uint17;

      procedure Set_Uint22 (N : Node_Id; Val : Uint) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 3).Field9 := To_Union (Val);
      end Set_Uint22;

      procedure Set_Ureal3 (N : Node_Id; Val : Ureal) is
      begin
         pragma Assert (N <= Nodes.Last);
         Nodes.Table (N).Field3 := To_Union (Val);
      end Set_Ureal3;

      procedure Set_Ureal18 (N : Node_Id; Val : Ureal) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).Field11 := To_Union (Val);
      end Set_Ureal18;

      procedure Set_Ureal21 (N : Node_Id; Val : Ureal) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 3).Field8 := To_Union (Val);
      end Set_Ureal21;

      procedure Set_Flag0 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (N <= Nodes.Last);
         Flags.Table (N).Flag0 := Val;
      end Set_Flag0;

      procedure Set_Flag1 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (N <= Nodes.Last);
         Flags.Table (N).Flag1 := Val;
      end Set_Flag1;

      procedure Set_Flag2 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (N <= Nodes.Last);
         Flags.Table (N).Flag2 := Val;
      end Set_Flag2;

      procedure Set_Flag3 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (N <= Nodes.Last);
         Flags.Table (N).Flag3 := Val;
      end Set_Flag3;

      procedure Set_Flag4 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (N <= Nodes.Last);
         Nodes.Table (N).Flag4 := Val;
      end Set_Flag4;

      procedure Set_Flag5 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (N <= Nodes.Last);
         Nodes.Table (N).Flag5 := Val;
      end Set_Flag5;

      procedure Set_Flag6 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (N <= Nodes.Last);
         Nodes.Table (N).Flag6 := Val;
      end Set_Flag6;

      procedure Set_Flag7 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (N <= Nodes.Last);
         Nodes.Table (N).Flag7 := Val;
      end Set_Flag7;

      procedure Set_Flag8 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (N <= Nodes.Last);
         Nodes.Table (N).Flag8 := Val;
      end Set_Flag8;

      procedure Set_Flag9 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (N <= Nodes.Last);
         Nodes.Table (N).Flag9 := Val;
      end Set_Flag9;

      procedure Set_Flag10 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (N <= Nodes.Last);
         Nodes.Table (N).Flag10 := Val;
      end Set_Flag10;

      procedure Set_Flag11 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (N <= Nodes.Last);
         Nodes.Table (N).Flag11 := Val;
      end Set_Flag11;

      procedure Set_Flag12 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (N <= Nodes.Last);
         Nodes.Table (N).Flag12 := Val;
      end Set_Flag12;

      procedure Set_Flag13 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (N <= Nodes.Last);
         Nodes.Table (N).Flag13 := Val;
      end Set_Flag13;

      procedure Set_Flag14 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (N <= Nodes.Last);
         Nodes.Table (N).Flag14 := Val;
      end Set_Flag14;

      procedure Set_Flag15 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (N <= Nodes.Last);
         Nodes.Table (N).Flag15 := Val;
      end Set_Flag15;

      procedure Set_Flag16 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (N <= Nodes.Last);
         Nodes.Table (N).Flag16 := Val;
      end Set_Flag16;

      procedure Set_Flag17 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (N <= Nodes.Last);
         Nodes.Table (N).Flag17 := Val;
      end Set_Flag17;

      procedure Set_Flag18 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (N <= Nodes.Last);
         Nodes.Table (N).Flag18 := Val;
      end Set_Flag18;

      procedure Set_Flag19 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).In_List := Val;
      end Set_Flag19;

      procedure Set_Flag20 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).Has_Aspects := Val;
      end Set_Flag20;

      procedure Set_Flag21 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).Rewrite_Ins := Val;
      end Set_Flag21;

      procedure Set_Flag22 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).Analyzed := Val;
      end Set_Flag22;

      procedure Set_Flag23 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).Comes_From_Source := Val;
      end Set_Flag23;

      procedure Set_Flag24 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).Error_Posted := Val;
      end Set_Flag24;

      procedure Set_Flag25 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).Flag4 := Val;
      end Set_Flag25;

      procedure Set_Flag26 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).Flag5 := Val;
      end Set_Flag26;

      procedure Set_Flag27 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).Flag6 := Val;
      end Set_Flag27;

      procedure Set_Flag28 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).Flag7 := Val;
      end Set_Flag28;

      procedure Set_Flag29 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).Flag8 := Val;
      end Set_Flag29;

      procedure Set_Flag30 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).Flag9 := Val;
      end Set_Flag30;

      procedure Set_Flag31 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).Flag10 := Val;
      end Set_Flag31;

      procedure Set_Flag32 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).Flag11 := Val;
      end Set_Flag32;

      procedure Set_Flag33 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).Flag12 := Val;
      end Set_Flag33;

      procedure Set_Flag34 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).Flag13 := Val;
      end Set_Flag34;

      procedure Set_Flag35 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).Flag14 := Val;
      end Set_Flag35;

      procedure Set_Flag36 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).Flag15 := Val;
      end Set_Flag36;

      procedure Set_Flag37 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).Flag16 := Val;
      end Set_Flag37;

      procedure Set_Flag38 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).Flag17 := Val;
      end Set_Flag38;

      procedure Set_Flag39 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).Flag18 := Val;
      end Set_Flag39;

      procedure Set_Flag40 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).In_List := Val;
      end Set_Flag40;

      procedure Set_Flag41 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).Has_Aspects := Val;
      end Set_Flag41;

      procedure Set_Flag42 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).Rewrite_Ins := Val;
      end Set_Flag42;

      procedure Set_Flag43 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).Analyzed := Val;
      end Set_Flag43;

      procedure Set_Flag44 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).Comes_From_Source := Val;
      end Set_Flag44;

      procedure Set_Flag45 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).Error_Posted := Val;
      end Set_Flag45;

      procedure Set_Flag46 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).Flag4 := Val;
      end Set_Flag46;

      procedure Set_Flag47 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).Flag5 := Val;
      end Set_Flag47;

      procedure Set_Flag48 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).Flag6 := Val;
      end Set_Flag48;

      procedure Set_Flag49 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).Flag7 := Val;
      end Set_Flag49;

      procedure Set_Flag50 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).Flag8 := Val;
      end Set_Flag50;

      procedure Set_Flag51 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).Flag9 := Val;
      end Set_Flag51;

      procedure Set_Flag52 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).Flag10 := Val;
      end Set_Flag52;

      procedure Set_Flag53 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).Flag11 := Val;
      end Set_Flag53;

      procedure Set_Flag54 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).Flag12 := Val;
      end Set_Flag54;

      procedure Set_Flag55 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).Flag13 := Val;
      end Set_Flag55;

      procedure Set_Flag56 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).Flag14 := Val;
      end Set_Flag56;

      procedure Set_Flag57 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).Flag15 := Val;
      end Set_Flag57;

      procedure Set_Flag58 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).Flag16 := Val;
      end Set_Flag58;

      procedure Set_Flag59 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).Flag17 := Val;
      end Set_Flag59;

      procedure Set_Flag60 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).Flag18 := Val;
      end Set_Flag60;

      procedure Set_Flag61 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).Pflag1 := Val;
      end Set_Flag61;

      procedure Set_Flag62 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 1).Pflag2 := Val;
      end Set_Flag62;

      procedure Set_Flag63 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).Pflag1 := Val;
      end Set_Flag63;

      procedure Set_Flag64 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 2).Pflag2 := Val;
      end Set_Flag64;

      procedure Set_Flag65 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Byte_Ptr
           (Node_Kind_Ptr'
             (Nodes.Table (N + 2).Nkind'Unrestricted_Access)).Flag65 := Val;
      end Set_Flag65;

      procedure Set_Flag66 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Byte_Ptr
           (Node_Kind_Ptr'
             (Nodes.Table (N + 2).Nkind'Unrestricted_Access)).Flag66 := Val;
      end Set_Flag66;

      procedure Set_Flag67 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Byte_Ptr
           (Node_Kind_Ptr'
             (Nodes.Table (N + 2).Nkind'Unrestricted_Access)).Flag67 := Val;
      end Set_Flag67;

      procedure Set_Flag68 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Byte_Ptr
           (Node_Kind_Ptr'
             (Nodes.Table (N + 2).Nkind'Unrestricted_Access)).Flag68 := Val;
      end Set_Flag68;

      procedure Set_Flag69 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Byte_Ptr
           (Node_Kind_Ptr'
             (Nodes.Table (N + 2).Nkind'Unrestricted_Access)).Flag69 := Val;
      end Set_Flag69;

      procedure Set_Flag70 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Byte_Ptr
           (Node_Kind_Ptr'
             (Nodes.Table (N + 2).Nkind'Unrestricted_Access)).Flag70 := Val;
      end Set_Flag70;

      procedure Set_Flag71 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Byte_Ptr
           (Node_Kind_Ptr'
             (Nodes.Table (N + 2).Nkind'Unrestricted_Access)).Flag71 := Val;
      end Set_Flag71;

      procedure Set_Flag72 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Byte_Ptr
           (Node_Kind_Ptr'
             (Nodes.Table (N + 2).Nkind'Unrestricted_Access)).Flag72 := Val;
      end Set_Flag72;

      procedure Set_Flag73 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 2).Field12'Unrestricted_Access)).Flag73 := Val;
      end Set_Flag73;

      procedure Set_Flag74 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 2).Field12'Unrestricted_Access)).Flag74 := Val;
      end Set_Flag74;

      procedure Set_Flag75 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 2).Field12'Unrestricted_Access)).Flag75 := Val;
      end Set_Flag75;

      procedure Set_Flag76 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 2).Field12'Unrestricted_Access)).Flag76 := Val;
      end Set_Flag76;

      procedure Set_Flag77 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 2).Field12'Unrestricted_Access)).Flag77 := Val;
      end Set_Flag77;

      procedure Set_Flag78 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 2).Field12'Unrestricted_Access)).Flag78 := Val;
      end Set_Flag78;

      procedure Set_Flag79 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 2).Field12'Unrestricted_Access)).Flag79 := Val;
      end Set_Flag79;

      procedure Set_Flag80 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 2).Field12'Unrestricted_Access)).Flag80 := Val;
      end Set_Flag80;

      procedure Set_Flag81 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 2).Field12'Unrestricted_Access)).Flag81 := Val;
      end Set_Flag81;

      procedure Set_Flag82 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 2).Field12'Unrestricted_Access)).Flag82 := Val;
      end Set_Flag82;

      procedure Set_Flag83 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 2).Field12'Unrestricted_Access)).Flag83 := Val;
      end Set_Flag83;

      procedure Set_Flag84 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 2).Field12'Unrestricted_Access)).Flag84 := Val;
      end Set_Flag84;

      procedure Set_Flag85 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 2).Field12'Unrestricted_Access)).Flag85 := Val;
      end Set_Flag85;

      procedure Set_Flag86 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 2).Field12'Unrestricted_Access)).Flag86 := Val;
      end Set_Flag86;

      procedure Set_Flag87 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 2).Field12'Unrestricted_Access)).Flag87 := Val;
      end Set_Flag87;

      procedure Set_Flag88 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 2).Field12'Unrestricted_Access)).Flag88 := Val;
      end Set_Flag88;

      procedure Set_Flag89 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 2).Field12'Unrestricted_Access)).Flag89 := Val;
      end Set_Flag89;

      procedure Set_Flag90 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 2).Field12'Unrestricted_Access)).Flag90 := Val;
      end Set_Flag90;

      procedure Set_Flag91 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 2).Field12'Unrestricted_Access)).Flag91 := Val;
      end Set_Flag91;

      procedure Set_Flag92 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 2).Field12'Unrestricted_Access)).Flag92 := Val;
      end Set_Flag92;

      procedure Set_Flag93 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 2).Field12'Unrestricted_Access)).Flag93 := Val;
      end Set_Flag93;

      procedure Set_Flag94 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 2).Field12'Unrestricted_Access)).Flag94 := Val;
      end Set_Flag94;

      procedure Set_Flag95 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 2).Field12'Unrestricted_Access)).Flag95 := Val;
      end Set_Flag95;

      procedure Set_Flag96 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 2).Field12'Unrestricted_Access)).Flag96 := Val;
      end Set_Flag96;

      procedure Set_Flag97 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word2_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field12'Unrestricted_Access)).Flag97 := Val;
      end Set_Flag97;

      procedure Set_Flag98 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word2_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field12'Unrestricted_Access)).Flag98 := Val;
      end Set_Flag98;

      procedure Set_Flag99 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word2_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field12'Unrestricted_Access)).Flag99 := Val;
      end Set_Flag99;

      procedure Set_Flag100 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word2_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field12'Unrestricted_Access)).Flag100 := Val;
      end Set_Flag100;

      procedure Set_Flag101 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word2_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field12'Unrestricted_Access)).Flag101 := Val;
      end Set_Flag101;

      procedure Set_Flag102 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word2_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field12'Unrestricted_Access)).Flag102 := Val;
      end Set_Flag102;

      procedure Set_Flag103 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word2_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field12'Unrestricted_Access)).Flag103 := Val;
      end Set_Flag103;

      procedure Set_Flag104 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word2_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field12'Unrestricted_Access)).Flag104 := Val;
      end Set_Flag104;

      procedure Set_Flag105 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word2_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field12'Unrestricted_Access)).Flag105 := Val;
      end Set_Flag105;

      procedure Set_Flag106 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word2_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field12'Unrestricted_Access)).Flag106 := Val;
      end Set_Flag106;

      procedure Set_Flag107 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word2_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field12'Unrestricted_Access)).Flag107 := Val;
      end Set_Flag107;

      procedure Set_Flag108 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word2_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field12'Unrestricted_Access)).Flag108 := Val;
      end Set_Flag108;

      procedure Set_Flag109 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word2_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field12'Unrestricted_Access)).Flag109 := Val;
      end Set_Flag109;

      procedure Set_Flag110 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word2_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field12'Unrestricted_Access)).Flag110 := Val;
      end Set_Flag110;

      procedure Set_Flag111 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word2_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field12'Unrestricted_Access)).Flag111 := Val;
      end Set_Flag111;

      procedure Set_Flag112 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word2_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field12'Unrestricted_Access)).Flag112 := Val;
      end Set_Flag112;

      procedure Set_Flag113 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word2_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field12'Unrestricted_Access)).Flag113 := Val;
      end Set_Flag113;

      procedure Set_Flag114 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word2_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field12'Unrestricted_Access)).Flag114 := Val;
      end Set_Flag114;

      procedure Set_Flag115 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word2_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field12'Unrestricted_Access)).Flag115 := Val;
      end Set_Flag115;

      procedure Set_Flag116 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word2_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field12'Unrestricted_Access)).Flag116 := Val;
      end Set_Flag116;

      procedure Set_Flag117 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word2_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field12'Unrestricted_Access)).Flag117 := Val;
      end Set_Flag117;

      procedure Set_Flag118 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word2_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field12'Unrestricted_Access)).Flag118 := Val;
      end Set_Flag118;

      procedure Set_Flag119 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word2_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field12'Unrestricted_Access)).Flag119 := Val;
      end Set_Flag119;

      procedure Set_Flag120 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word2_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field12'Unrestricted_Access)).Flag120 := Val;
      end Set_Flag120;

      procedure Set_Flag121 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word2_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field12'Unrestricted_Access)).Flag121 := Val;
      end Set_Flag121;

      procedure Set_Flag122 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word2_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field12'Unrestricted_Access)).Flag122 := Val;
      end Set_Flag122;

      procedure Set_Flag123 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word2_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field12'Unrestricted_Access)).Flag123 := Val;
      end Set_Flag123;

      procedure Set_Flag124 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word2_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field12'Unrestricted_Access)).Flag124 := Val;
      end Set_Flag124;

      procedure Set_Flag125 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word2_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field12'Unrestricted_Access)).Flag125 := Val;
      end Set_Flag125;

      procedure Set_Flag126 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word2_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field12'Unrestricted_Access)).Flag126 := Val;
      end Set_Flag126;

      procedure Set_Flag127 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word2_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field12'Unrestricted_Access)).Flag127 := Val;
      end Set_Flag127;

      procedure Set_Flag128 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word2_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field12'Unrestricted_Access)).Flag128 := Val;
      end Set_Flag128;

      procedure Set_Flag129 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 3).In_List := Val;
      end Set_Flag129;

      procedure Set_Flag130 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 3).Has_Aspects := Val;
      end Set_Flag130;

      procedure Set_Flag131 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 3).Rewrite_Ins := Val;
      end Set_Flag131;

      procedure Set_Flag132 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 3).Analyzed := Val;
      end Set_Flag132;

      procedure Set_Flag133 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 3).Comes_From_Source := Val;
      end Set_Flag133;

      procedure Set_Flag134 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 3).Error_Posted := Val;
      end Set_Flag134;

      procedure Set_Flag135 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 3).Flag4 := Val;
      end Set_Flag135;

      procedure Set_Flag136 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 3).Flag5 := Val;
      end Set_Flag136;

      procedure Set_Flag137 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 3).Flag6 := Val;
      end Set_Flag137;

      procedure Set_Flag138 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 3).Flag7 := Val;
      end Set_Flag138;

      procedure Set_Flag139 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 3).Flag8 := Val;
      end Set_Flag139;

      procedure Set_Flag140 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 3).Flag9 := Val;
      end Set_Flag140;

      procedure Set_Flag141 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 3).Flag10 := Val;
      end Set_Flag141;

      procedure Set_Flag142 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 3).Flag11 := Val;
      end Set_Flag142;

      procedure Set_Flag143 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 3).Flag12 := Val;
      end Set_Flag143;

      procedure Set_Flag144 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 3).Flag13 := Val;
      end Set_Flag144;

      procedure Set_Flag145 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 3).Flag14 := Val;
      end Set_Flag145;

      procedure Set_Flag146 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 3).Flag15 := Val;
      end Set_Flag146;

      procedure Set_Flag147 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 3).Flag16 := Val;
      end Set_Flag147;

      procedure Set_Flag148 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 3).Flag17 := Val;
      end Set_Flag148;

      procedure Set_Flag149 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 3).Flag18 := Val;
      end Set_Flag149;

      procedure Set_Flag150 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 3).Pflag1 := Val;
      end Set_Flag150;

      procedure Set_Flag151 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 3).Pflag2 := Val;
      end Set_Flag151;

      procedure Set_Flag152 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word3_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field11'Unrestricted_Access)).Flag152 := Val;
      end Set_Flag152;

      procedure Set_Flag153 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word3_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field11'Unrestricted_Access)).Flag153 := Val;
      end Set_Flag153;

      procedure Set_Flag154 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word3_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field11'Unrestricted_Access)).Flag154 := Val;
      end Set_Flag154;

      procedure Set_Flag155 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word3_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field11'Unrestricted_Access)).Flag155 := Val;
      end Set_Flag155;

      procedure Set_Flag156 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word3_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field11'Unrestricted_Access)).Flag156 := Val;
      end Set_Flag156;

      procedure Set_Flag157 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word3_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field11'Unrestricted_Access)).Flag157 := Val;
      end Set_Flag157;

      procedure Set_Flag158 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word3_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field11'Unrestricted_Access)).Flag158 := Val;
      end Set_Flag158;

      procedure Set_Flag159 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word3_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field11'Unrestricted_Access)).Flag159 := Val;
      end Set_Flag159;

      procedure Set_Flag160 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word3_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field11'Unrestricted_Access)).Flag160 := Val;
      end Set_Flag160;

      procedure Set_Flag161 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word3_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field11'Unrestricted_Access)).Flag161 := Val;
      end Set_Flag161;

      procedure Set_Flag162 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word3_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field11'Unrestricted_Access)).Flag162 := Val;
      end Set_Flag162;

      procedure Set_Flag163 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word3_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field11'Unrestricted_Access)).Flag163 := Val;
      end Set_Flag163;

      procedure Set_Flag164 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word3_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field11'Unrestricted_Access)).Flag164 := Val;
      end Set_Flag164;

      procedure Set_Flag165 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word3_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field11'Unrestricted_Access)).Flag165 := Val;
      end Set_Flag165;

      procedure Set_Flag166 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word3_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field11'Unrestricted_Access)).Flag166 := Val;
      end Set_Flag166;

      procedure Set_Flag167 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word3_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field11'Unrestricted_Access)).Flag167 := Val;
      end Set_Flag167;

      procedure Set_Flag168 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word3_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field11'Unrestricted_Access)).Flag168 := Val;
      end Set_Flag168;

      procedure Set_Flag169 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word3_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field11'Unrestricted_Access)).Flag169 := Val;
      end Set_Flag169;

      procedure Set_Flag170 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word3_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field11'Unrestricted_Access)).Flag170 := Val;
      end Set_Flag170;

      procedure Set_Flag171 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word3_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field11'Unrestricted_Access)).Flag171 := Val;
      end Set_Flag171;

      procedure Set_Flag172 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word3_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field11'Unrestricted_Access)).Flag172 := Val;
      end Set_Flag172;

      procedure Set_Flag173 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word3_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field11'Unrestricted_Access)).Flag173 := Val;
      end Set_Flag173;

      procedure Set_Flag174 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word3_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field11'Unrestricted_Access)).Flag174 := Val;
      end Set_Flag174;

      procedure Set_Flag175 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word3_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field11'Unrestricted_Access)).Flag175 := Val;
      end Set_Flag175;

      procedure Set_Flag176 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word3_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field11'Unrestricted_Access)).Flag176 := Val;
      end Set_Flag176;

      procedure Set_Flag177 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word3_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field11'Unrestricted_Access)).Flag177 := Val;
      end Set_Flag177;

      procedure Set_Flag178 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word3_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field11'Unrestricted_Access)).Flag178 := Val;
      end Set_Flag178;

      procedure Set_Flag179 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word3_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field11'Unrestricted_Access)).Flag179 := Val;
      end Set_Flag179;

      procedure Set_Flag180 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word3_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field11'Unrestricted_Access)).Flag180 := Val;
      end Set_Flag180;

      procedure Set_Flag181 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word3_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field11'Unrestricted_Access)).Flag181 := Val;
      end Set_Flag181;

      procedure Set_Flag182 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word3_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field11'Unrestricted_Access)).Flag182 := Val;
      end Set_Flag182;

      procedure Set_Flag183 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word3_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 3).Field11'Unrestricted_Access)).Flag183 := Val;
      end Set_Flag183;

      procedure Set_Flag184 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word4_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 4).Field12'Unrestricted_Access)).Flag184 := Val;
      end Set_Flag184;

      procedure Set_Flag185 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word4_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 4).Field12'Unrestricted_Access)).Flag185 := Val;
      end Set_Flag185;

      procedure Set_Flag186 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word4_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 4).Field12'Unrestricted_Access)).Flag186 := Val;
      end Set_Flag186;

      procedure Set_Flag187 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word4_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 4).Field12'Unrestricted_Access)).Flag187 := Val;
      end Set_Flag187;

      procedure Set_Flag188 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word4_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 4).Field12'Unrestricted_Access)).Flag188 := Val;
      end Set_Flag188;

      procedure Set_Flag189 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word4_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 4).Field12'Unrestricted_Access)).Flag189 := Val;
      end Set_Flag189;

      procedure Set_Flag190 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word4_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 4).Field12'Unrestricted_Access)).Flag190 := Val;
      end Set_Flag190;

      procedure Set_Flag191 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word4_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 4).Field12'Unrestricted_Access)).Flag191 := Val;
      end Set_Flag191;

      procedure Set_Flag192 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word4_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 4).Field12'Unrestricted_Access)).Flag192 := Val;
      end Set_Flag192;

      procedure Set_Flag193 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word4_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 4).Field12'Unrestricted_Access)).Flag193 := Val;
      end Set_Flag193;

      procedure Set_Flag194 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word4_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 4).Field12'Unrestricted_Access)).Flag194 := Val;
      end Set_Flag194;

      procedure Set_Flag195 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word4_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 4).Field12'Unrestricted_Access)).Flag195 := Val;
      end Set_Flag195;

      procedure Set_Flag196 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word4_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 4).Field12'Unrestricted_Access)).Flag196 := Val;
      end Set_Flag196;

      procedure Set_Flag197 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word4_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 4).Field12'Unrestricted_Access)).Flag197 := Val;
      end Set_Flag197;

      procedure Set_Flag198 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word4_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 4).Field12'Unrestricted_Access)).Flag198 := Val;
      end Set_Flag198;

      procedure Set_Flag199 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word4_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 4).Field12'Unrestricted_Access)).Flag199 := Val;
      end Set_Flag199;

      procedure Set_Flag200 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word4_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 4).Field12'Unrestricted_Access)).Flag200 := Val;
      end Set_Flag200;

      procedure Set_Flag201 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word4_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 4).Field12'Unrestricted_Access)).Flag201 := Val;
      end Set_Flag201;

      procedure Set_Flag202 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word4_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 4).Field12'Unrestricted_Access)).Flag202 := Val;
      end Set_Flag202;

      procedure Set_Flag203 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word4_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 4).Field12'Unrestricted_Access)).Flag203 := Val;
      end Set_Flag203;

      procedure Set_Flag204 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word4_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 4).Field12'Unrestricted_Access)).Flag204 := Val;
      end Set_Flag204;

      procedure Set_Flag205 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word4_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 4).Field12'Unrestricted_Access)).Flag205 := Val;
      end Set_Flag205;

      procedure Set_Flag206 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word4_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 4).Field12'Unrestricted_Access)).Flag206 := Val;
      end Set_Flag206;

      procedure Set_Flag207 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word4_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 4).Field12'Unrestricted_Access)).Flag207 := Val;
      end Set_Flag207;

      procedure Set_Flag208 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word4_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 4).Field12'Unrestricted_Access)).Flag208 := Val;
      end Set_Flag208;

      procedure Set_Flag209 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word4_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 4).Field12'Unrestricted_Access)).Flag209 := Val;
      end Set_Flag209;

      procedure Set_Flag210 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word4_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 4).Field12'Unrestricted_Access)).Flag210 := Val;
      end Set_Flag210;

      procedure Set_Flag211 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word4_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 4).Field12'Unrestricted_Access)).Flag211 := Val;
      end Set_Flag211;

      procedure Set_Flag212 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word4_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 4).Field12'Unrestricted_Access)).Flag212 := Val;
      end Set_Flag212;

      procedure Set_Flag213 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word4_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 4).Field12'Unrestricted_Access)).Flag213 := Val;
      end Set_Flag213;

      procedure Set_Flag214 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word4_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 4).Field12'Unrestricted_Access)).Flag214 := Val;
      end Set_Flag214;

      procedure Set_Flag215 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word4_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 4).Field12'Unrestricted_Access)).Flag215 := Val;
      end Set_Flag215;

      procedure Set_Flag216 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 4).In_List := Val;
      end Set_Flag216;

      procedure Set_Flag217 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 4).Has_Aspects := Val;
      end Set_Flag217;

      procedure Set_Flag218 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 4).Rewrite_Ins := Val;
      end Set_Flag218;

      procedure Set_Flag219 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 4).Analyzed := Val;
      end Set_Flag219;

      procedure Set_Flag220 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 4).Comes_From_Source := Val;
      end Set_Flag220;

      procedure Set_Flag221 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 4).Error_Posted := Val;
      end Set_Flag221;

      procedure Set_Flag222 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 4).Flag4 := Val;
      end Set_Flag222;

      procedure Set_Flag223 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 4).Flag5 := Val;
      end Set_Flag223;

      procedure Set_Flag224 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 4).Flag6 := Val;
      end Set_Flag224;

      procedure Set_Flag225 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 4).Flag7 := Val;
      end Set_Flag225;

      procedure Set_Flag226 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 4).Flag8 := Val;
      end Set_Flag226;

      procedure Set_Flag227 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 4).Flag9 := Val;
      end Set_Flag227;

      procedure Set_Flag228 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 4).Flag10 := Val;
      end Set_Flag228;

      procedure Set_Flag229 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 4).Flag11 := Val;
      end Set_Flag229;

      procedure Set_Flag230 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 4).Flag12 := Val;
      end Set_Flag230;

      procedure Set_Flag231 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 4).Flag13 := Val;
      end Set_Flag231;

      procedure Set_Flag232 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 4).Flag14 := Val;
      end Set_Flag232;

      procedure Set_Flag233 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 4).Flag15 := Val;
      end Set_Flag233;

      procedure Set_Flag234 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 4).Flag16 := Val;
      end Set_Flag234;

      procedure Set_Flag235 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 4).Flag17 := Val;
      end Set_Flag235;

      procedure Set_Flag236 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 4).Flag18 := Val;
      end Set_Flag236;

      procedure Set_Flag237 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 4).Pflag1 := Val;
      end Set_Flag237;

      procedure Set_Flag238 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 4).Pflag2 := Val;
      end Set_Flag238;

      procedure Set_Flag239 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Byte2_Ptr
           (Node_Kind_Ptr'
             (Nodes.Table (N + 3).Nkind'Unrestricted_Access)).Flag239 := Val;
      end Set_Flag239;

      procedure Set_Flag240 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Byte2_Ptr
           (Node_Kind_Ptr'
             (Nodes.Table (N + 3).Nkind'Unrestricted_Access)).Flag240 := Val;
      end Set_Flag240;

      procedure Set_Flag241 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Byte2_Ptr
           (Node_Kind_Ptr'
             (Nodes.Table (N + 3).Nkind'Unrestricted_Access)).Flag241 := Val;
      end Set_Flag241;

      procedure Set_Flag242 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Byte2_Ptr
           (Node_Kind_Ptr'
             (Nodes.Table (N + 3).Nkind'Unrestricted_Access)).Flag242 := Val;
      end Set_Flag242;

      procedure Set_Flag243 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Byte2_Ptr
           (Node_Kind_Ptr'
             (Nodes.Table (N + 3).Nkind'Unrestricted_Access)).Flag243 := Val;
      end Set_Flag243;

      procedure Set_Flag244 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Byte2_Ptr
           (Node_Kind_Ptr'
             (Nodes.Table (N + 3).Nkind'Unrestricted_Access)).Flag244 := Val;
      end Set_Flag244;

      procedure Set_Flag245 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Byte2_Ptr
           (Node_Kind_Ptr'
             (Nodes.Table (N + 3).Nkind'Unrestricted_Access)).Flag245 := Val;
      end Set_Flag245;

      procedure Set_Flag246 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Byte2_Ptr
           (Node_Kind_Ptr'
             (Nodes.Table (N + 3).Nkind'Unrestricted_Access)).Flag246 := Val;
      end Set_Flag246;

      procedure Set_Flag247 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Byte3_Ptr
           (Node_Kind_Ptr'
             (Nodes.Table (N + 4).Nkind'Unrestricted_Access)).Flag247 := Val;
      end Set_Flag247;

      procedure Set_Flag248 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Byte3_Ptr
           (Node_Kind_Ptr'
             (Nodes.Table (N + 4).Nkind'Unrestricted_Access)).Flag248 := Val;
      end Set_Flag248;

      procedure Set_Flag249 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Byte3_Ptr
           (Node_Kind_Ptr'
             (Nodes.Table (N + 4).Nkind'Unrestricted_Access)).Flag249 := Val;
      end Set_Flag249;

      procedure Set_Flag250 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Byte3_Ptr
           (Node_Kind_Ptr'
             (Nodes.Table (N + 4).Nkind'Unrestricted_Access)).Flag250 := Val;
      end Set_Flag250;

      procedure Set_Flag251 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Byte3_Ptr
           (Node_Kind_Ptr'
             (Nodes.Table (N + 4).Nkind'Unrestricted_Access)).Flag251 := Val;
      end Set_Flag251;

      procedure Set_Flag252 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Byte3_Ptr
           (Node_Kind_Ptr'
             (Nodes.Table (N + 4).Nkind'Unrestricted_Access)).Flag252 := Val;
      end Set_Flag252;

      procedure Set_Flag253 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Byte3_Ptr
           (Node_Kind_Ptr'
             (Nodes.Table (N + 4).Nkind'Unrestricted_Access)).Flag253 := Val;
      end Set_Flag253;

      procedure Set_Flag254 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Byte3_Ptr
           (Node_Kind_Ptr'
             (Nodes.Table (N + 4).Nkind'Unrestricted_Access)).Flag254 := Val;
      end Set_Flag254;

      procedure Set_Flag255 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word5_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 5).Field12'Unrestricted_Access)).Flag255 := Val;
      end Set_Flag255;

      procedure Set_Flag256 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word5_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 5).Field12'Unrestricted_Access)).Flag256 := Val;
      end Set_Flag256;

      procedure Set_Flag257 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word5_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 5).Field12'Unrestricted_Access)).Flag257 := Val;
      end Set_Flag257;

      procedure Set_Flag258 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word5_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 5).Field12'Unrestricted_Access)).Flag258 := Val;
      end Set_Flag258;

      procedure Set_Flag259 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word5_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 5).Field12'Unrestricted_Access)).Flag259 := Val;
      end Set_Flag259;

      procedure Set_Flag260 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word5_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 5).Field12'Unrestricted_Access)).Flag260 := Val;
      end Set_Flag260;

      procedure Set_Flag261 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word5_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 5).Field12'Unrestricted_Access)).Flag261 := Val;
      end Set_Flag261;

      procedure Set_Flag262 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word5_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 5).Field12'Unrestricted_Access)).Flag262 := Val;
      end Set_Flag262;

      procedure Set_Flag263 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word5_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 5).Field12'Unrestricted_Access)).Flag263 := Val;
      end Set_Flag263;

      procedure Set_Flag264 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word5_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 5).Field12'Unrestricted_Access)).Flag264 := Val;
      end Set_Flag264;

      procedure Set_Flag265 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word5_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 5).Field12'Unrestricted_Access)).Flag265 := Val;
      end Set_Flag265;

      procedure Set_Flag266 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word5_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 5).Field12'Unrestricted_Access)).Flag266 := Val;
      end Set_Flag266;

      procedure Set_Flag267 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word5_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 5).Field12'Unrestricted_Access)).Flag267 := Val;
      end Set_Flag267;

      procedure Set_Flag268 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word5_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 5).Field12'Unrestricted_Access)).Flag268 := Val;
      end Set_Flag268;

      procedure Set_Flag269 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word5_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 5).Field12'Unrestricted_Access)).Flag269 := Val;
      end Set_Flag269;

      procedure Set_Flag270 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word5_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 5).Field12'Unrestricted_Access)).Flag270 := Val;
      end Set_Flag270;

      procedure Set_Flag271 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word5_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 5).Field12'Unrestricted_Access)).Flag271 := Val;
      end Set_Flag271;

      procedure Set_Flag272 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word5_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 5).Field12'Unrestricted_Access)).Flag272 := Val;
      end Set_Flag272;

      procedure Set_Flag273 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word5_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 5).Field12'Unrestricted_Access)).Flag273 := Val;
      end Set_Flag273;

      procedure Set_Flag274 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word5_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 5).Field12'Unrestricted_Access)).Flag274 := Val;
      end Set_Flag274;

      procedure Set_Flag275 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word5_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 5).Field12'Unrestricted_Access)).Flag275 := Val;
      end Set_Flag275;

      procedure Set_Flag276 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word5_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 5).Field12'Unrestricted_Access)).Flag276 := Val;
      end Set_Flag276;

      procedure Set_Flag277 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word5_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 5).Field12'Unrestricted_Access)).Flag277 := Val;
      end Set_Flag277;

      procedure Set_Flag278 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word5_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 5).Field12'Unrestricted_Access)).Flag278 := Val;
      end Set_Flag278;

      procedure Set_Flag279 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word5_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 5).Field12'Unrestricted_Access)).Flag279 := Val;
      end Set_Flag279;

      procedure Set_Flag280 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word5_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 5).Field12'Unrestricted_Access)).Flag280 := Val;
      end Set_Flag280;

      procedure Set_Flag281 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word5_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 5).Field12'Unrestricted_Access)).Flag281 := Val;
      end Set_Flag281;

      procedure Set_Flag282 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word5_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 5).Field12'Unrestricted_Access)).Flag282 := Val;
      end Set_Flag282;

      procedure Set_Flag283 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word5_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 5).Field12'Unrestricted_Access)).Flag283 := Val;
      end Set_Flag283;

      procedure Set_Flag284 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word5_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 5).Field12'Unrestricted_Access)).Flag284 := Val;
      end Set_Flag284;

      procedure Set_Flag285 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word5_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 5).Field12'Unrestricted_Access)).Flag285 := Val;
      end Set_Flag285;

      procedure Set_Flag286 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Word5_Ptr
           (Union_Id_Ptr'
             (Nodes.Table (N + 5).Field12'Unrestricted_Access)).Flag286 := Val;
      end Set_Flag286;

      procedure Set_Flag287 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 5).In_List := Val;
      end Set_Flag287;

      procedure Set_Flag288 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 5).Has_Aspects := Val;
      end Set_Flag288;

      procedure Set_Flag289 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 5).Rewrite_Ins := Val;
      end Set_Flag289;

      procedure Set_Flag290 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 5).Analyzed := Val;
      end Set_Flag290;

      procedure Set_Flag291 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 5).Comes_From_Source := Val;
      end Set_Flag291;

      procedure Set_Flag292 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 5).Error_Posted := Val;
      end Set_Flag292;

      procedure Set_Flag293 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 5).Flag4 := Val;
      end Set_Flag293;

      procedure Set_Flag294 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 5).Flag5 := Val;
      end Set_Flag294;

      procedure Set_Flag295 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 5).Flag6 := Val;
      end Set_Flag295;

      procedure Set_Flag296 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 5).Flag7 := Val;
      end Set_Flag296;

      procedure Set_Flag297 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 5).Flag8 := Val;
      end Set_Flag297;

      procedure Set_Flag298 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 5).Flag9 := Val;
      end Set_Flag298;

      procedure Set_Flag299 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 5).Flag10 := Val;
      end Set_Flag299;

      procedure Set_Flag300 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 5).Flag11 := Val;
      end Set_Flag300;

      procedure Set_Flag301 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 5).Flag12 := Val;
      end Set_Flag301;

      procedure Set_Flag302 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 5).Flag13 := Val;
      end Set_Flag302;

      procedure Set_Flag303 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 5).Flag14 := Val;
      end Set_Flag303;

      procedure Set_Flag304 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 5).Flag15 := Val;
      end Set_Flag304;

      procedure Set_Flag305 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 5).Flag16 := Val;
      end Set_Flag305;

      procedure Set_Flag306 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 5).Flag17 := Val;
      end Set_Flag306;

      procedure Set_Flag307 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 5).Flag18 := Val;
      end Set_Flag307;

      procedure Set_Flag308 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 5).Pflag1 := Val;
      end Set_Flag308;

      procedure Set_Flag309 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         Nodes.Table (N + 5).Pflag2 := Val;
      end Set_Flag309;

      procedure Set_Flag310 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Byte4_Ptr
           (Node_Kind_Ptr'
             (Nodes.Table (N + 5).Nkind'Unrestricted_Access)).Flag310 := Val;
      end Set_Flag310;

      procedure Set_Flag311 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Byte4_Ptr
           (Node_Kind_Ptr'
             (Nodes.Table (N + 5).Nkind'Unrestricted_Access)).Flag311 := Val;
      end Set_Flag311;

      procedure Set_Flag312 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Byte4_Ptr
           (Node_Kind_Ptr'
             (Nodes.Table (N + 5).Nkind'Unrestricted_Access)).Flag312 := Val;
      end Set_Flag312;

      procedure Set_Flag313 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Byte4_Ptr
           (Node_Kind_Ptr'
             (Nodes.Table (N + 5).Nkind'Unrestricted_Access)).Flag313 := Val;
      end Set_Flag313;

      procedure Set_Flag314 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Byte4_Ptr
           (Node_Kind_Ptr'
             (Nodes.Table (N + 5).Nkind'Unrestricted_Access)).Flag314 := Val;
      end Set_Flag314;

      procedure Set_Flag315 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Byte4_Ptr
           (Node_Kind_Ptr'
             (Nodes.Table (N + 5).Nkind'Unrestricted_Access)).Flag315 := Val;
      end Set_Flag315;

      procedure Set_Flag316 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Byte4_Ptr
           (Node_Kind_Ptr'
             (Nodes.Table (N + 5).Nkind'Unrestricted_Access)).Flag316 := Val;
      end Set_Flag316;

      procedure Set_Flag317 (N : Node_Id; Val : Boolean) is
      begin
         pragma Assert (Nkind (N) in N_Entity);
         To_Flag_Byte4_Ptr
           (Node_Kind_Ptr'
             (Nodes.Table (N + 5).Nkind'Unrestricted_Access)).Flag317 := Val;
      end Set_Flag317;

      procedure Set_Node1_With_Parent (N : Node_Id; Val : Node_Id) is
      begin
         pragma Assert (N <= Nodes.Last);

         if Val > Error then
            Set_Parent (N => Val, Val => N);
         end if;

         Set_Node1 (N, Val);
      end Set_Node1_With_Parent;

      procedure Set_Node2_With_Parent (N : Node_Id; Val : Node_Id) is
      begin
         pragma Assert (N <= Nodes.Last);

         if Val > Error then
            Set_Parent (N => Val, Val => N);
         end if;

         Set_Node2 (N, Val);
      end Set_Node2_With_Parent;

      procedure Set_Node3_With_Parent (N : Node_Id; Val : Node_Id) is
      begin
         pragma Assert (N <= Nodes.Last);

         if Val > Error then
            Set_Parent (N => Val, Val => N);
         end if;

         Set_Node3 (N, Val);
      end Set_Node3_With_Parent;

      procedure Set_Node4_With_Parent (N : Node_Id; Val : Node_Id) is
      begin
         pragma Assert (N <= Nodes.Last);

         if Val > Error then
            Set_Parent (N => Val, Val => N);
         end if;

         Set_Node4 (N, Val);
      end Set_Node4_With_Parent;

      procedure Set_Node5_With_Parent (N : Node_Id; Val : Node_Id) is
      begin
         pragma Assert (N <= Nodes.Last);

         if Val > Error then
            Set_Parent (N => Val, Val => N);
         end if;

         Set_Node5 (N, Val);
      end Set_Node5_With_Parent;

      procedure Set_List1_With_Parent (N : Node_Id; Val : List_Id) is
      begin
         pragma Assert (N <= Nodes.Last);
         if Val /= No_List and then Val /= Error_List then
            Set_Parent (Val, N);
         end if;
         Set_List1 (N, Val);
      end Set_List1_With_Parent;

      procedure Set_List2_With_Parent (N : Node_Id; Val : List_Id) is
      begin
         pragma Assert (N <= Nodes.Last);
         if Val /= No_List and then Val /= Error_List then
            Set_Parent (Val, N);
         end if;
         Set_List2 (N, Val);
      end Set_List2_With_Parent;

      procedure Set_List3_With_Parent (N : Node_Id; Val : List_Id) is
      begin
         pragma Assert (N <= Nodes.Last);
         if Val /= No_List and then Val /= Error_List then
            Set_Parent (Val, N);
         end if;
         Set_List3 (N, Val);
      end Set_List3_With_Parent;

      procedure Set_List4_With_Parent (N : Node_Id; Val : List_Id) is
      begin
         pragma Assert (N <= Nodes.Last);
         if Val /= No_List and then Val /= Error_List then
            Set_Parent (Val, N);
         end if;
         Set_List4 (N, Val);
      end Set_List4_With_Parent;

      procedure Set_List5_With_Parent (N : Node_Id; Val : List_Id) is
      begin
         pragma Assert (N <= Nodes.Last);
         if Val /= No_List and then Val /= Error_List then
            Set_Parent (Val, N);
         end if;
         Set_List5 (N, Val);
      end Set_List5_With_Parent;

   end Unchecked_Access;

   ------------
   -- Unlock --
   ------------

   procedure Unlock is
   begin
      Nodes.Locked := False;
      Flags.Locked := False;
      Orig_Nodes.Locked := False;
   end Unlock;

end Atree;
