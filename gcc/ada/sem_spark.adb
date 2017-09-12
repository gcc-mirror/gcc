------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                            S E M _ S P A R K                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2017, Free Software Foundation, Inc.              --
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

with Atree;    use Atree;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Opt;      use Opt;
with Osint;    use Osint;
with Sem_Prag; use Sem_Prag;
with Sem_Util; use Sem_Util;
with Sem_Aux;  use Sem_Aux;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Treepr;   use Treepr;

with Ada.Unchecked_Deallocation;
with GNAT.Dynamic_HTables; use GNAT.Dynamic_HTables;

package body Sem_SPARK is

   -------------------------------------------------
   -- Handling of Permissions Associated to Paths --
   -------------------------------------------------

   package Permissions is
      Elaboration_Context_Max : constant := 1009;
      --  The hash range

      type Elaboration_Context_Index is range 0 .. Elaboration_Context_Max - 1;

      function Elaboration_Context_Hash
        (Key : Entity_Id) return Elaboration_Context_Index;
      --  Function to hash any node of the AST

      type Perm_Kind is (No_Access, Read_Only, Read_Write, Write_Only);
      --  Permission type associated with paths

      subtype Read_Perm is Perm_Kind range Read_Only .. Read_Write;
      subtype Write_Perm is Perm_Kind range Read_Write .. Write_Only;

      type Perm_Tree_Wrapper;

      type Perm_Tree_Access is access Perm_Tree_Wrapper;
      --  A tree of permissions is defined, where the root is a whole object
      --  and tree branches follow access paths in memory. As Perm_Tree is a
      --  discriminated record, a wrapper type is used for the access type
      --  designating a subtree, to make it unconstrained so that it can be
      --  updated.

      --  Nodes in the permission tree are of different kinds

      type Path_Kind is
        (Entire_Object,    --  Scalar object, or folded object of any type
         Reference,        --  Unfolded object of access type
         Array_Component,  --  Unfolded object of array type
         Record_Component  --  Unfolded object of record type
        );

      package Perm_Tree_Maps is new Simple_HTable
        (Header_Num => Elaboration_Context_Index,
         Key        => Node_Id,
         Element    => Perm_Tree_Access,
         No_Element => null,
         Hash       => Elaboration_Context_Hash,
         Equal      => "=");
      --  The instantation of a hash table, with keys being nodes and values
      --  being pointers to trees. This is used to reference easily all
      --  extensions of a Record_Component node (that can have name x, y, ...).

      --  The definition of permission trees. This is a tree, which has a
      --  permission at each node, and depending on the type of the node,
      --  can have zero, one, or more children pointed to by an access to tree.
      type Perm_Tree (Kind : Path_Kind := Entire_Object) is record
         Permission : Perm_Kind;
         --  Permission at this level in the path

         Is_Node_Deep : Boolean;
         --  Whether this node is of a deep type, to be used when moving the
         --  path.

         case Kind is

            --  An entire object is either a leaf (an object which cannot be
            --  extended further in a path) or a subtree in folded form (which
            --  could later be unfolded further in another kind of node). The
            --  field Children_Permission specifies a permission for every
            --  extension of that node if that permission is different from
            --  the node's permission.

            when Entire_Object    =>
               Children_Permission : Perm_Kind;

            --  Unfolded path of access type. The permission of the object
            --  pointed to is given in Get_All.

            when Reference        =>
               Get_All : Perm_Tree_Access;

            --  Unfolded path of array type. The permission of the elements is
            --  given in Get_Elem.

            when Array_Component  =>
               Get_Elem : Perm_Tree_Access;

            --  Unfolded path of record type. The permission of the regular
            --  components is given in Component. The permission of unknown
            --  components (for objects of tagged type) is given in
            --  Other_Components.

            when Record_Component =>
               Component : Perm_Tree_Maps.Instance;
               Other_Components : Perm_Tree_Access;
         end case;
      end record;

      type Perm_Tree_Wrapper is record
         Tree : Perm_Tree;
      end record;
      --  We use this wrapper in order to have unconstrained discriminants

      type Perm_Env is new Perm_Tree_Maps.Instance;
      --  The definition of a permission environment for the analysis. This
      --  is just a hash table of permission trees, each of them rooted with
      --  an Identifier/Expanded_Name.

      type Perm_Env_Access is access Perm_Env;
      --  Access to permission environments

      package Env_Maps is new Simple_HTable
        (Header_Num => Elaboration_Context_Index,
         Key        => Entity_Id,
         Element    => Perm_Env_Access,
         No_Element => null,
         Hash       => Elaboration_Context_Hash,
         Equal      => "=");
      --  The instantiation of a hash table whose elements are permission
      --  environments. This hash table is used to save the environments at
      --  the entry of each loop, with the key being the loop label.

      type Env_Backups is new Env_Maps.Instance;
      --  The type defining the hash table saving the environments at the entry
      --  of each loop.

      package Boolean_Variables_Maps is new Simple_HTable
        (Header_Num => Elaboration_Context_Index,
         Key        => Entity_Id,
         Element    => Boolean,
         No_Element => False,
         Hash       => Elaboration_Context_Hash,
         Equal      => "=");
      --  These maps allow tracking the variables that have been declared but
      --  never used anywhere in the source code. Especially, we do not raise
      --  an error if the variable stays write-only and is declared at package
      --  level, because there is no risk that the variable has been moved,
      --  because it has never been used.

      type Initialization_Map is new Boolean_Variables_Maps.Instance;

      --------------------
      -- Simple Getters --
      --------------------

      --  Simple getters to avoid having .all.Tree.Field everywhere. Of course,
      --  that's only for the top access, as otherwise this reverses the order
      --  in accesses visually.

      function Children_Permission (T : Perm_Tree_Access) return Perm_Kind;
      function Component (T : Perm_Tree_Access) return Perm_Tree_Maps.Instance;
      function Get_All (T : Perm_Tree_Access) return Perm_Tree_Access;
      function Get_Elem (T : Perm_Tree_Access) return Perm_Tree_Access;
      function Is_Node_Deep (T : Perm_Tree_Access) return Boolean;
      function Kind (T : Perm_Tree_Access) return Path_Kind;
      function Other_Components (T : Perm_Tree_Access) return Perm_Tree_Access;
      function Permission (T : Perm_Tree_Access) return Perm_Kind;

      -----------------------
      -- Memory Management --
      -----------------------

      procedure Copy_Env
        (From : Perm_Env;
         To : in out Perm_Env);
      --  Procedure to copy a permission environment

      procedure Copy_Init_Map
        (From : Initialization_Map;
         To : in out Initialization_Map);
      --  Procedure to copy an initialization map

      procedure Copy_Tree
        (From : Perm_Tree_Access;
         To : Perm_Tree_Access);
      --  Procedure to copy a permission tree

      procedure Free_Env
        (PE : in out Perm_Env);
      --  Procedure to free a permission environment

      procedure Free_Perm_Tree
        (PT : in out Perm_Tree_Access);
      --  Procedure to free a permission tree

      --------------------
      -- Error Messages --
      --------------------

      procedure Perm_Mismatch
        (Exp_Perm, Act_Perm : Perm_Kind;
         N                   : Node_Id);
      --  Issues a continuation error message about a mismatch between a
      --  desired permission Exp_Perm and a permission obtained Act_Perm. N
      --  is the node on which the error is reported.

   end Permissions;

   package body Permissions is

      -------------------------
      -- Children_Permission --
      -------------------------

      function Children_Permission
        (T : Perm_Tree_Access)
          return Perm_Kind
      is
      begin
         return T.all.Tree.Children_Permission;
      end Children_Permission;

      ---------------
      -- Component --
      ---------------

      function Component
        (T : Perm_Tree_Access)
          return Perm_Tree_Maps.Instance
      is
      begin
         return T.all.Tree.Component;
      end Component;

      --------------
      -- Copy_Env --
      --------------

      procedure Copy_Env
        (From : Perm_Env;
         To : in out Perm_Env)
      is
         Comp_From : Perm_Tree_Access;
         Key_From : Perm_Tree_Maps.Key_Option;
         Son : Perm_Tree_Access;

      begin
         Reset (To);
         Key_From := Get_First_Key (From);
         while Key_From.Present loop
            Comp_From := Get (From, Key_From.K);
            pragma Assert (Comp_From /= null);

            Son := new Perm_Tree_Wrapper;
            Copy_Tree (Comp_From, Son);

            Set (To, Key_From.K, Son);
            Key_From := Get_Next_Key (From);
         end loop;
      end Copy_Env;

      -------------------
      -- Copy_Init_Map --
      -------------------

      procedure Copy_Init_Map
        (From : Initialization_Map;
         To : in out Initialization_Map)
      is
         Comp_From : Boolean;
         Key_From : Boolean_Variables_Maps.Key_Option;

      begin
         Reset (To);
         Key_From := Get_First_Key (From);
         while Key_From.Present loop
            Comp_From := Get (From, Key_From.K);
            Set (To, Key_From.K, Comp_From);
            Key_From := Get_Next_Key (From);
         end loop;
      end Copy_Init_Map;

      ---------------
      -- Copy_Tree --
      ---------------

      procedure Copy_Tree
        (From : Perm_Tree_Access;
         To : Perm_Tree_Access)
      is
      begin
         To.all := From.all;

         case Kind (From) is
            when Entire_Object =>
               null;

            when Reference =>
               To.all.Tree.Get_All := new Perm_Tree_Wrapper;

               Copy_Tree (Get_All (From), Get_All (To));

            when Array_Component =>
               To.all.Tree.Get_Elem := new Perm_Tree_Wrapper;

               Copy_Tree (Get_Elem (From), Get_Elem (To));

            when Record_Component =>
               declare
                  Comp_From : Perm_Tree_Access;
                  Key_From : Perm_Tree_Maps.Key_Option;
                  Son : Perm_Tree_Access;
                  Hash_Table : Perm_Tree_Maps.Instance;
               begin
               --  We put a new hash table, so that it gets dealiased from the
               --  Component (From) hash table.
                  To.all.Tree.Component := Hash_Table;

                  To.all.Tree.Other_Components :=
                    new Perm_Tree_Wrapper'(Other_Components (From).all);

                  Copy_Tree (Other_Components (From), Other_Components (To));

                  Key_From := Perm_Tree_Maps.Get_First_Key
                    (Component (From));
                  while Key_From.Present loop
                     Comp_From := Perm_Tree_Maps.Get
                       (Component (From), Key_From.K);

                     pragma Assert (Comp_From /= null);
                     Son := new Perm_Tree_Wrapper;

                     Copy_Tree (Comp_From, Son);

                     Perm_Tree_Maps.Set
                       (To.all.Tree.Component, Key_From.K, Son);

                     Key_From := Perm_Tree_Maps.Get_Next_Key
                       (Component (From));
                  end loop;
               end;
         end case;
      end Copy_Tree;

      ------------------------------
      -- Elaboration_Context_Hash --
      ------------------------------

      function Elaboration_Context_Hash
        (Key : Entity_Id) return Elaboration_Context_Index
      is
      begin
         return Elaboration_Context_Index (Key mod Elaboration_Context_Max);
      end Elaboration_Context_Hash;

      --------------
      -- Free_Env --
      --------------

      procedure Free_Env (PE : in out Perm_Env) is
         CompO : Perm_Tree_Access;
      begin
         CompO := Get_First (PE);
         while CompO /= null loop
            Free_Perm_Tree (CompO);
            CompO := Get_Next (PE);
         end loop;
      end Free_Env;

      --------------------
      -- Free_Perm_Tree --
      --------------------

      procedure Free_Perm_Tree
        (PT : in out Perm_Tree_Access)
      is
         procedure Free_Perm_Tree_Dealloc is
           new Ada.Unchecked_Deallocation
             (Perm_Tree_Wrapper, Perm_Tree_Access);
         --  The deallocator for permission_trees

      begin
         case Kind (PT) is
            when Entire_Object =>
               Free_Perm_Tree_Dealloc (PT);

            when Reference =>
               Free_Perm_Tree (PT.all.Tree.Get_All);
               Free_Perm_Tree_Dealloc (PT);

            when Array_Component =>
               Free_Perm_Tree (PT.all.Tree.Get_Elem);

            when Record_Component =>
               declare
                  Comp : Perm_Tree_Access;

               begin
                  Free_Perm_Tree (PT.all.Tree.Other_Components);
                  Comp := Perm_Tree_Maps.Get_First (Component (PT));
                  while Comp /= null loop
                     --  Free every Component subtree

                     Free_Perm_Tree (Comp);
                     Comp := Perm_Tree_Maps.Get_Next (Component (PT));
                  end loop;
               end;
               Free_Perm_Tree_Dealloc (PT);
         end case;
      end Free_Perm_Tree;

      -------------
      -- Get_All --
      -------------

      function Get_All
        (T : Perm_Tree_Access)
          return Perm_Tree_Access
      is
      begin
         return T.all.Tree.Get_All;
      end Get_All;

      --------------
      -- Get_Elem --
      --------------

      function Get_Elem
        (T : Perm_Tree_Access)
          return Perm_Tree_Access
      is
      begin
         return T.all.Tree.Get_Elem;
      end Get_Elem;

      ------------------
      -- Is_Node_Deep --
      ------------------

      function Is_Node_Deep
        (T : Perm_Tree_Access)
          return Boolean
      is
      begin
         return T.all.Tree.Is_Node_Deep;
      end Is_Node_Deep;

      ----------
      -- Kind --
      ----------

      function Kind
        (T : Perm_Tree_Access)
          return Path_Kind
      is
      begin
         return T.all.Tree.Kind;
      end Kind;

      ----------------------
      -- Other_Components --
      ----------------------

      function Other_Components
        (T : Perm_Tree_Access)
          return Perm_Tree_Access
      is
      begin
         return T.all.Tree.Other_Components;
      end Other_Components;

      ----------------
      -- Permission --
      ----------------

      function Permission
        (T : Perm_Tree_Access)
          return Perm_Kind
      is
      begin
         return T.all.Tree.Permission;
      end Permission;

      -------------------
      -- Perm_Mismatch --
      -------------------

      procedure Perm_Mismatch
        (Exp_Perm, Act_Perm : Perm_Kind;
         N                   : Node_Id)
      is
      begin
         Error_Msg_N ("\expected at least `"
                      & Perm_Kind'Image (Exp_Perm) & "`, got `"
                      & Perm_Kind'Image (Act_Perm) & "`", N);
      end Perm_Mismatch;

   end Permissions;

   use Permissions;

   --------------------------------------
   -- Analysis modes for AST traversal --
   --------------------------------------

   --  The different modes for analysis. This allows to checking whether a path
   --  found in the code should be moved, borrowed, or observed.

   type Checking_Mode is

     (Read,
      --  Default mode. Checks that paths have Read_Perm permission.

      Move,
      --  Regular moving semantics (not under 'Access). Checks that paths have
      --  Read_Write permission. After moving a path, its permission is set to
      --  Write_Only and the permission of its extensions is set to No_Access.

      Assign,
      --  Used for the target of an assignment, or an actual parameter with
      --  mode OUT. Checks that paths have Write_Perm permission. After
      --  assigning to a path, its permission is set to Read_Write.

      Super_Move,
      --  Enhanced moving semantics (under 'Access). Checks that paths have
      --  Read_Write permission. After moving a path, its permission is set
      --  to No_Access, as well as the permission of its extensions and the
      --  permission of its prefixes up to the first Reference node.

      Borrow_Out,
      --  Used for actual OUT parameters. Checks that paths have Write_Perm
      --  permission. After checking a path, its permission is set to Read_Only
      --  when of a by-copy type, to No_Access otherwise. After the call, its
      --  permission is set to Read_Write.

      Observe
      --  Used for actual IN parameters of a scalar type. Checks that paths
      --  have Read_Perm permission. After checking a path, its permission
      --  is set to Read_Only.
      --
      --  Also used for formal IN parameters
     );

   type Result_Kind is (Folded, Unfolded, Function_Call);
   --  The type declaration to discriminate in the Perm_Or_Tree type

   --  The result type of the function Get_Perm_Or_Tree. This returns either a
   --  tree when it found the appropriate tree, or a permission when the search
   --  finds a leaf and the subtree we are looking for is folded. In the last
   --  case, we return instead the Children_Permission field of the leaf.

   type Perm_Or_Tree (R : Result_Kind) is record
      case R is
         when Folded        => Found_Permission : Perm_Kind;
         when Unfolded      => Tree_Access : Perm_Tree_Access;
         when Function_Call => null;
      end case;
   end record;

   -----------------------
   -- Local subprograms --
   -----------------------

   function "<=" (P1, P2 : Perm_Kind) return Boolean;
   function ">=" (P1, P2 : Perm_Kind) return Boolean;
   function Glb  (P1, P2 : Perm_Kind) return Perm_Kind;
   function Lub  (P1, P2 : Perm_Kind) return Perm_Kind;

   --  Checking proceduress for safe pointer usage. These procedures traverse
   --  the AST, check nodes for correct permissions according to SPARK RM
   --  6.4.2, and update permissions depending on the node kind.

   procedure Check_Call_Statement (Call : Node_Id);

   procedure Check_Callable_Body (Body_N : Node_Id);
   --  We are not in End_Of_Callee mode, hence we will save the environment
   --  and start from a new one. We will add in the environment all formal
   --  parameters as well as global used during the subprogram, with the
   --  appropriate permissions (write-only for out, read-only for observed,
   --  read-write for others).
   --
   --  After that we analyze the body of the function, and finaly, we check
   --  that each borrowed parameter and global has read-write permission. We
   --  then clean up the environment and put back the saved environment.

   procedure Check_Declaration (Decl : Node_Id);

   procedure Check_Expression (Expr : Node_Id);

   procedure Check_Globals (N : Node_Id; Check_Mode : Checking_Mode);
   --  This procedure takes a global pragma and checks depending on mode:
   --  Mode Read: every in global is readable
   --  Mode Observe: same as Check_Param_Observes but on globals
   --  Mode Borrow_Out: Check_Param_Outs for globals
   --  Mode Move: Check_Param for globals with mode Read
   --  Mode Assign: Check_Param for globals with mode Assign

   procedure Check_List (L : List_Id);
   --  Calls Check_Node on each element of the list

   procedure Check_Loop_Statement (Loop_N : Node_Id);

   procedure Check_Node (N : Node_Id);
   --  Main traversal procedure to check safe pointer usage. This procedure is
   --  mutually recursive with the specialized procedures that follow.

   procedure Check_Package_Body (Pack : Node_Id);

   procedure Check_Param (Formal : Entity_Id; Actual : Node_Id);
   --  This procedure takes a formal and an actual parameter and calls the
   --  analyze node if the parameter is borrowed with mode in out, with the
   --  appropriate Checking_Mode (Move).

   procedure Check_Param_Observes (Formal : Entity_Id; Actual : Node_Id);
   --  This procedure takes a formal and an actual parameter and calls
   --  the analyze node if the parameter is observed, with the appropriate
   --  Checking_Mode.

   procedure Check_Param_Outs (Formal : Entity_Id; Actual : Node_Id);
   --  This procedure takes a formal and an actual parameter and calls the
   --  analyze node if the parameter is of mode out, with the appropriate
   --  Checking_Mode.

   procedure Check_Param_Read (Formal : Entity_Id; Actual : Node_Id);
   --  This procedure takes a formal and an actual parameter and checks the
   --  readability of every in-mode parameter. This includes observed in, and
   --  also borrowed in, that are then checked afterwards.

   procedure Check_Statement (Stmt : Node_Id);

   function Get_Perm (N : Node_Id) return Perm_Kind;
   --  The function that takes a name as input and returns a permission
   --  associated to it.

   function Get_Perm_Or_Tree (N : Node_Id) return Perm_Or_Tree;
   --  This function gets a Node_Id and looks recursively to find the
   --  appropriate subtree for that Node_Id. If the tree is folded on
   --  that node, then it returns the permission given at the right level.

   function Get_Perm_Tree (N : Node_Id) return Perm_Tree_Access;
   --  This function gets a Node_Id and looks recursively to find the
   --  appropriate subtree for that Node_Id. If the tree is folded, then
   --  it unrolls the tree up to the appropriate level.

   function Has_Alias
     (N : Node_Id)
       return Boolean;
   --  Function that returns whether the path given as parameter contains an
   --  extension that is declared as aliased.

   function Has_Array_Component (N : Node_Id) return Boolean;
   --  This function gets a Node_Id and looks recursively to find if the given
   --  path has any array component.

   function Has_Function_Component (N : Node_Id) return Boolean;
   --  This function gets a Node_Id and looks recursively to find if the given
   --  path has any function component.

   procedure Hp (P : Perm_Env);
   --  A procedure that outputs the hash table. This function is used only in
   --  the debugger to look into a hash table.
   pragma Unreferenced (Hp);

   procedure Illegal_Global_Usage (N : Node_Or_Entity_Id);
   pragma No_Return (Illegal_Global_Usage);
   --  A procedure that is called when deep globals or aliased globals are used
   --  without any global aspect.

   function Is_Borrowed_In (E : Entity_Id) return Boolean;
   --  Function that tells if the given entity is a borrowed in a formal
   --  parameter, that is, if it is an access-to-variable type.

   function Is_Deep (E : Entity_Id) return Boolean;
   --  A function that can tell if a type is deep or not. Returns true if the
   --  type passed as argument is deep.

   function Is_Shallow (E : Entity_Id) return Boolean;
   --  A function that can tell if a type is shallow or not. Returns true if
   --  the type passed as argument is shallow.

   function Loop_Of_Exit (N : Node_Id) return Entity_Id;
   --  A function that takes an exit statement node and returns the entity of
   --  the loop that this statement is exiting from.

   procedure Merge_Envs (Target : in out Perm_Env; Source : in out Perm_Env);
   --  Merge Target and Source into Target, and then deallocate the Source

   procedure Perm_Error
     (N : Node_Id;
      Perm : Perm_Kind;
      Found_Perm : Perm_Kind);
   --  A procedure that is called when the permissions found contradict the
   --  rules established by the RM. This function is called with the node, its
   --  entity and the permission that was expected, and adds an error message
   --  with the appropriate values.

   procedure Perm_Error_Subprogram_End
     (E          : Entity_Id;
      Subp       : Entity_Id;
      Perm       : Perm_Kind;
      Found_Perm : Perm_Kind);
   --  A procedure that is called when the permissions found contradict the
   --  rules established by the RM at the end of subprograms. This function
   --  is called with the node, its entity, the node of the returning function
   --  and the permission that was expected, and adds an error message with the
   --  appropriate values.

   procedure Process_Path (N : Node_Id);

   procedure Return_Declarations (L : List_Id);
   --  Check correct permissions on every declared object at the end of a
   --  callee. Used at the end of the body of a callable entity. Checks that
   --  paths of all borrowed formal parameters and global have Read_Write
   --  permission.

   procedure Return_Globals (Subp : Entity_Id);
   --  Takes a subprogram as input, and checks that all borrowed global items
   --  of the subprogram indeed have RW permission at the end of the subprogram
   --  execution.

   procedure Return_Parameter_Or_Global
     (Id   : Entity_Id;
      Mode : Formal_Kind;
      Subp : Entity_Id);
   --  Auxiliary procedure to Return_Parameters and Return_Globals

   procedure Return_Parameters (Subp : Entity_Id);
   --  Takes a subprogram as input, and checks that all borrowed parameters of
   --  the subprogram indeed have RW permission at the end of the subprogram
   --  execution.

   procedure Set_Perm_Extensions (T : Perm_Tree_Access; P : Perm_Kind);
   --  This procedure takes an access to a permission tree and modifies the
   --  tree so that any strict extensions of the given tree become of the
   --  access specified by parameter P.

   procedure Set_Perm_Extensions_Move (T : Perm_Tree_Access; E : Entity_Id);
   --  Set permissions to
   --    No for any extension with more .all
   --    W for any deep extension with same number of .all
   --    RW for any shallow extension with same number of .all

   function Set_Perm_Prefixes_Assign (N : Node_Id) return Perm_Tree_Access;
   --  This function takes a name as an input and sets in the permission
   --  tree the given permission to the given name. The general rule here is
   --  that everybody updates the permission of the subtree it is returning.
   --  The permission of the assigned path has been set to RW by the caller.
   --
   --  Case where we have to normalize a tree after the correct permissions
   --  have been assigned already. We look for the right subtree at the given
   --  path, actualize its permissions, and then call the normalization on its
   --  parent.
   --
   --  Example: We assign x.y and x.z then during Set_Perm_Prefixes_Move will
   --  change the permission of x to RW because all of its components have
   --  permission have permission RW.

   function Set_Perm_Prefixes_Borrow_Out (N : Node_Id) return Perm_Tree_Access;
   --  This function modifies the permissions of a given node_id in the
   --  permission environment as well as in all the prefixes of the path,
   --  given that the path is borrowed with mode out.

   function Set_Perm_Prefixes_Move
     (N : Node_Id; Mode : Checking_Mode)
      return Perm_Tree_Access;
   pragma Precondition (Mode = Move or Mode = Super_Move);
   --  Given a node and a mode (that can be either Move or Super_Move), this
   --  function modifies the permissions of a given node_id in the permission
   --  environment as well as all the prefixes of the path, given that the path
   --  is moved with or without 'Access. The general rule here is everybody
   --  updates the permission of the subtree they are returning.
   --
   --  This case describes a move either under 'Access or without 'Access.

   function Set_Perm_Prefixes_Observe (N : Node_Id) return Perm_Tree_Access;
   --  This function modifies the permissions of a given node_id in the
   --  permission environment as well as all the prefixes of the path,
   --  given that the path is observed.

   procedure Setup_Globals (Subp : Entity_Id);
   --  Takes a subprogram as input, and sets up the environment by adding
   --  global items with appropriate permissions.

   procedure Setup_Parameter_Or_Global
     (Id   : Entity_Id;
      Mode : Formal_Kind);
   --  Auxiliary procedure to Setup_Parameters and Setup_Globals

   procedure Setup_Parameters (Subp : Entity_Id);
   --  Takes a subprogram as input, and sets up the environment by adding
   --  formal parameters with appropriate permissions.

   ----------------------
   -- Global Variables --
   ----------------------

   Current_Perm_Env : Perm_Env;
   --  The permission environment that is used for the analysis. This
   --  environment can be saved, modified, reinitialized, but should be the
   --  only one valid from which to extract the permissions of the paths in
   --  scope. The analysis ensures at each point that this variables contains
   --  a valid permission environment with all bindings in scope.

   Current_Checking_Mode : Checking_Mode := Read;
   --  The current analysis mode. This global variable indicates at each point
   --  of the analysis whether the node being analyzed is moved, borrowed,
   --  assigned, read, ... The full list of possible values can be found in
   --  the declaration of type Checking_Mode.

   Current_Loops_Envs : Env_Backups;
   --  This variable contains saves of permission environments at each loop the
   --  analysis entered. Each saved environment can be reached with the label
   --  of the loop.

   Current_Loops_Accumulators : Env_Backups;
   --  This variable contains the environments used as accumulators for loops,
   --  that consist of the merge of all environments at each exit point of
   --  the loop (which can also be the entry point of the loop in the case of
   --  non-infinite loops), each of them reachable from the label of the loop.
   --  We require that the environment stored in the accumulator be less
   --  restrictive than the saved environment at the beginning of the loop, and
   --  the permission environment after the loop is equal to the accumulator.

   Current_Initialization_Map : Initialization_Map;
   --  This variable contains a map that binds each variable of the analyzed
   --  source code to a boolean that becomes true whenever the variable is used
   --  after declaration. Hence we can exclude from analysis variables that
   --  are just declared and never accessed, typically at package declaration.

   ----------
   -- "<=" --
   ----------

   function "<=" (P1, P2 : Perm_Kind) return Boolean
   is
   begin
      return P2 >= P1;
   end "<=";

   ----------
   -- ">=" --
   ----------

   function ">=" (P1, P2 : Perm_Kind) return Boolean
   is
   begin
      case P2 is
         when No_Access  => return True;
         when Read_Only  => return P1 in Read_Perm;
         when Write_Only => return P1 in Write_Perm;
         when Read_Write => return P1 = Read_Write;
      end case;
   end ">=";

   --------------------------
   -- Check_Call_Statement --
   --------------------------

   procedure Check_Call_Statement (Call : Node_Id) is
      Saved_Env : Perm_Env;

      procedure Iterate_Call is new
        Iterate_Call_Parameters (Check_Param);
      procedure Iterate_Call_Observes is new
        Iterate_Call_Parameters (Check_Param_Observes);
      procedure Iterate_Call_Outs is new
        Iterate_Call_Parameters (Check_Param_Outs);
      procedure Iterate_Call_Read is new
        Iterate_Call_Parameters (Check_Param_Read);

   begin
      --  Save environment, so that the modifications done by analyzing the
      --  parameters are not kept at the end of the call.
      Copy_Env (Current_Perm_Env,
                Saved_Env);

      --  We first check the Read access on every in parameter

      Current_Checking_Mode := Read;
      Iterate_Call_Read (Call);
      Check_Globals (Get_Pragma
                       (Get_Called_Entity (Call), Pragma_Global), Read);

      --  We first observe, then borrow with mode out, and then with other
      --  modes. This ensures that we do not have to check for by-copy types
      --  specially, because we read them before borrowing them.

      Iterate_Call_Observes (Call);
      Check_Globals (Get_Pragma
                       (Get_Called_Entity (Call), Pragma_Global),
                       Observe);

      Iterate_Call_Outs (Call);
      Check_Globals (Get_Pragma
                       (Get_Called_Entity (Call), Pragma_Global),
                       Borrow_Out);

      Iterate_Call (Call);
      Check_Globals (Get_Pragma
                       (Get_Called_Entity (Call), Pragma_Global), Move);

      --  Restore environment, because after borrowing/observing actual
      --  parameters, they get their permission reverted to the ones before
      --  the call.

      Free_Env (Current_Perm_Env);

      Copy_Env (Saved_Env,
                Current_Perm_Env);

      Free_Env (Saved_Env);

      --  We assign the out parameters (necessarily borrowed per RM)
      Current_Checking_Mode := Assign;
      Iterate_Call (Call);
      Check_Globals (Get_Pragma
                       (Get_Called_Entity (Call), Pragma_Global),
                       Assign);

   end Check_Call_Statement;

   -------------------------
   -- Check_Callable_Body --
   -------------------------

   procedure Check_Callable_Body (Body_N : Node_Id) is

      Mode_Before : constant Checking_Mode := Current_Checking_Mode;

      Saved_Env : Perm_Env;
      Saved_Init_Map : Initialization_Map;

      New_Env : Perm_Env;

      Body_Id : constant Entity_Id := Defining_Entity (Body_N);
      Spec_Id : constant Entity_Id := Unique_Entity (Body_Id);

   begin
      --  Check if SPARK pragma is not set to Off

      if Present (SPARK_Pragma (Defining_Entity (Body_N))) then
         if Get_SPARK_Mode_From_Annotation
           (SPARK_Pragma (Defining_Entity (Body_N, False))) /= Opt.On
         then
            return;
         end if;
      else
         return;
      end if;

      --  Save environment and put a new one in place

      Copy_Env (Current_Perm_Env, Saved_Env);

      --  Save initialization map

      Copy_Init_Map (Current_Initialization_Map, Saved_Init_Map);

      Current_Checking_Mode := Read;
      Current_Perm_Env := New_Env;

      --  Add formals and globals to the environment with adequate permissions

      if Is_Subprogram_Or_Entry (Spec_Id) then
         Setup_Parameters (Spec_Id);
         Setup_Globals (Spec_Id);
      end if;

      --  Analyze the body of the function

      Check_List (Declarations (Body_N));
      Check_Node (Handled_Statement_Sequence (Body_N));

      --  Check the read-write permissions of borrowed parameters/globals

      if Ekind_In (Spec_Id, E_Procedure, E_Entry)
        and then not No_Return (Spec_Id)
      then
         Return_Parameters (Spec_Id);
         Return_Globals (Spec_Id);
      end if;

      --  Free the environments

      Free_Env (Current_Perm_Env);

      Copy_Env (Saved_Env,
                Current_Perm_Env);

      Free_Env (Saved_Env);

      --  Restore initialization map

      Copy_Init_Map (Saved_Init_Map, Current_Initialization_Map);

      Reset (Saved_Init_Map);

      --  The assignment of all out parameters will be done by caller

      Current_Checking_Mode := Mode_Before;
   end Check_Callable_Body;

   -----------------------
   -- Check_Declaration --
   -----------------------

   procedure Check_Declaration (Decl : Node_Id) is
   begin
      case N_Declaration'(Nkind (Decl)) is
         when N_Full_Type_Declaration =>
            --  Nothing to do here ??? NOT TRUE IF CONSTRAINT ON TYPE
            null;

         when N_Object_Declaration =>
            --  First move the right-hand side
            Current_Checking_Mode := Move;
            Check_Node (Expression (Decl));

            declare
               Elem : Perm_Tree_Access;

            begin
               Elem := new Perm_Tree_Wrapper'
                 (Tree =>
                    (Kind                => Entire_Object,
                     Is_Node_Deep        =>
                       Is_Deep (Etype (Defining_Identifier (Decl))),
                     Permission          => Read_Write,
                     Children_Permission => Read_Write));

               --  If unitialized declaration, then set to Write_Only. If a
               --  pointer declaration, it has a null default initialization.
               if Nkind (Expression (Decl)) = N_Empty
                 and then not Has_Full_Default_Initialization
                   (Etype (Defining_Identifier (Decl)))
                 and then not Is_Access_Type
                   (Etype (Defining_Identifier (Decl)))
               then
                  Elem.all.Tree.Permission := Write_Only;
                  Elem.all.Tree.Children_Permission := Write_Only;
               end if;

               --  Create new tree for defining identifier

               Set (Current_Perm_Env,
                    Unique_Entity (Defining_Identifier (Decl)),
                    Elem);

               pragma Assert (Get_First (Current_Perm_Env)
                              /= null);

            end;

         when N_Subtype_Declaration =>
            Check_Node (Subtype_Indication (Decl));

         when N_Iterator_Specification =>
            pragma Assert (Is_Shallow (Etype (Defining_Identifier (Decl))));
            null;

         when N_Loop_Parameter_Specification =>
            pragma Assert (Is_Shallow (Etype (Defining_Identifier (Decl))));
            null;

         --  Checking should not be called directly on these nodes

         when N_Component_Declaration
            | N_Function_Specification
            | N_Entry_Declaration
            | N_Procedure_Specification
         =>
            raise Program_Error;

         --  Ignored constructs for pointer checking

         when N_Formal_Object_Declaration
            | N_Formal_Type_Declaration
            | N_Incomplete_Type_Declaration
            | N_Private_Extension_Declaration
            | N_Private_Type_Declaration
            | N_Protected_Type_Declaration
         =>
            null;

         --  The following nodes are rewritten by semantic analysis

         when N_Expression_Function =>
            raise Program_Error;
      end case;
   end Check_Declaration;

   ----------------------
   -- Check_Expression --
   ----------------------

   procedure Check_Expression (Expr : Node_Id) is
      Mode_Before : constant Checking_Mode := Current_Checking_Mode;
   begin
      case N_Subexpr'(Nkind (Expr)) is
         when N_Procedure_Call_Statement =>
            Check_Call_Statement (Expr);

         when N_Identifier
            | N_Expanded_Name
         =>
            --  Check if identifier is pointing to nothing (On/Off/...)
            if not Present (Entity (Expr)) then
               return;
            end if;

            --  Do not analyze things that are not of object Kind
            if Ekind (Entity (Expr)) not in Object_Kind then
               return;
            end if;

            --  Consider as ident
            Process_Path (Expr);

         --  Switch to read mode and then check the readability of each operand

         when N_Binary_Op =>

            Current_Checking_Mode := Read;
            Check_Node (Left_Opnd (Expr));
            Check_Node (Right_Opnd (Expr));

         --  Switch to read mode and then check the readability of each operand

         when N_Op_Abs
            | N_Op_Minus
            | N_Op_Not
            | N_Op_Plus
         =>
            pragma Assert (Is_Shallow (Etype (Expr)));
            Current_Checking_Mode := Read;
            Check_Node (Right_Opnd (Expr));

         --  Forbid all deep expressions for Attribute ???

         when N_Attribute_Reference =>
            case Attribute_Name (Expr) is
               when Name_Access =>
                  case Current_Checking_Mode is
                     when Read =>
                        Check_Node (Prefix (Expr));

                     when Move =>
                        Current_Checking_Mode := Super_Move;
                        Check_Node (Prefix (Expr));

                     --  Only assign names, not expressions

                     when Assign =>
                        raise Program_Error;

                     --  Prefix in Super_Move should be a name, error here

                     when Super_Move =>
                        raise Program_Error;

                     --  Could only borrow names of mode out, not n'Access

                     when Borrow_Out =>
                        raise Program_Error;

                     when Observe =>
                        Check_Node (Prefix (Expr));
                  end case;

               when Name_Last
                  | Name_First
               =>
                  Current_Checking_Mode := Read;
                  Check_Node (Prefix (Expr));

               when Name_Min =>
                  Current_Checking_Mode := Read;
                  Check_Node (Prefix (Expr));

               when Name_Image =>
                  Check_Node (Prefix (Expr));

               when Name_SPARK_Mode =>
                  null;

               when Name_Value =>
                  Current_Checking_Mode := Read;
                  Check_Node (Prefix (Expr));

               when Name_Update =>
                  Check_List (Expressions (Expr));
                  Check_Node (Prefix (Expr));

               when Name_Pred
                   | Name_Succ
               =>
                  Check_List (Expressions (Expr));
                  Check_Node (Prefix (Expr));

               when Name_Length =>
                  Current_Checking_Mode := Read;
                  Check_Node (Prefix (Expr));

               --  Any Attribute referring to the underlying memory is ignored
               --  in the analysis. This means that taking the address of a
               --  variable makes a silent alias that is not rejected by the
               --  analysis.

               when Name_Address
                   | Name_Alignment
                   | Name_Component_Size
                   | Name_First_Bit
                   | Name_Last_Bit
                   | Name_Size
                   | Name_Position
               =>
                  null;

               --  Attributes referring to types (get values from types), hence
               --  no need to check either for borrows or any loans.

               when Name_Base
                  | Name_Val
               =>
                  null;

               --  Other attributes that fall out of the scope of the analysis

               when others =>
                  null;
            end case;

         when N_In =>
            Current_Checking_Mode := Read;
            Check_Node (Left_Opnd (Expr));
            Check_Node (Right_Opnd (Expr));

         when N_Not_In =>
            Current_Checking_Mode := Read;
            Check_Node (Left_Opnd (Expr));
            Check_Node (Right_Opnd (Expr));

         --  Switch to read mode and then check the readability of each operand

         when N_And_Then
            | N_Or_Else
         =>
            pragma Assert (Is_Shallow (Etype (Expr)));
            Current_Checking_Mode := Read;
            Check_Node (Left_Opnd (Expr));
            Check_Node (Right_Opnd (Expr));

         --  Check the arguments of the call

         when N_Function_Call =>
            Current_Checking_Mode := Read;
            Check_List (Parameter_Associations (Expr));

         when N_Explicit_Dereference =>
            Process_Path (Expr);

         --  Copy environment, run on each branch, and then merge

         when N_If_Expression =>
            declare
               Saved_Env : Perm_Env;

               --  Accumulator for the different branches

               New_Env : Perm_Env;

               Elmt : Node_Id := First (Expressions (Expr));

            begin
               Current_Checking_Mode := Read;

               Check_Node (Elmt);

               Current_Checking_Mode := Mode_Before;

               --  Save environment

               Copy_Env (Current_Perm_Env,
                                 Saved_Env);

               --  Here we have the original env in saved, current with a fresh
               --  copy, and new aliased.

               --  THEN PART

               Next (Elmt);
               Check_Node (Elmt);

               --  Here the new_environment contains curr env after then block

               --  ELSE part

               --  Restore environment before if
               Copy_Env (Current_Perm_Env,
                                 New_Env);

               Free_Env (Current_Perm_Env);

               Copy_Env (Saved_Env,
                                 Current_Perm_Env);

               --  Here new environment contains the environment after then and
               --  current the fresh copy of old one.

               Next (Elmt);
               Check_Node (Elmt);

               Merge_Envs (New_Env,
                                   Current_Perm_Env);

               --  CLEANUP

               Copy_Env (New_Env,
                                 Current_Perm_Env);

               Free_Env (New_Env);
               Free_Env (Saved_Env);
            end;

         when N_Indexed_Component =>
            Process_Path (Expr);

         --  Analyze the expression that is getting qualified

         when N_Qualified_Expression =>
            Check_Node (Expression (Expr));

         when N_Quantified_Expression =>
            declare
               Saved_Env : Perm_Env;
            begin
               Copy_Env (Current_Perm_Env, Saved_Env);
               Current_Checking_Mode := Read;
               Check_Node (Iterator_Specification (Expr));
               Check_Node (Loop_Parameter_Specification (Expr));

               Check_Node (Condition (Expr));
               Free_Env (Current_Perm_Env);
               Copy_Env (Saved_Env, Current_Perm_Env);
               Free_Env (Saved_Env);
            end;

         --  Analyze the list of associations in the aggregate

         when N_Aggregate =>
            Check_List (Expressions (Expr));
            Check_List (Component_Associations (Expr));

         when N_Allocator =>
            Check_Node (Expression (Expr));

         when N_Case_Expression =>
            declare
               Saved_Env : Perm_Env;

               --  Accumulator for the different branches

               New_Env : Perm_Env;

               Elmt : Node_Id := First (Alternatives (Expr));

            begin
               Current_Checking_Mode := Read;
               Check_Node (Expression (Expr));

               Current_Checking_Mode := Mode_Before;

               --  Save environment

               Copy_Env (Current_Perm_Env,
                                 Saved_Env);

               --  Here we have the original env in saved, current with a fresh
               --  copy, and new aliased.

               --  First alternative

               Check_Node (Elmt);
               Next (Elmt);

               Copy_Env (Current_Perm_Env,
                                 New_Env);

               Free_Env (Current_Perm_Env);

               --  Other alternatives

               while Present (Elmt) loop
                  --  Restore environment

                  Copy_Env (Saved_Env,
                                    Current_Perm_Env);

                  Check_Node (Elmt);

                  --  Merge Current_Perm_Env into New_Env

                  Merge_Envs (New_Env,
                                      Current_Perm_Env);

                  Next (Elmt);
               end loop;

               --  CLEANUP
               Copy_Env (New_Env,
                                 Current_Perm_Env);

               Free_Env (New_Env);
               Free_Env (Saved_Env);
            end;

         --  Analyze the list of associates in the aggregate as well as the
         --  ancestor part.

         when N_Extension_Aggregate =>

            Check_Node (Ancestor_Part (Expr));
            Check_List (Expressions (Expr));

         when N_Range =>
            Check_Node (Low_Bound (Expr));
            Check_Node (High_Bound (Expr));

         --  We arrived at a path. Process it.

         when N_Selected_Component =>
            Process_Path (Expr);

         when N_Slice =>
            Process_Path (Expr);

         when N_Type_Conversion =>
            Check_Node (Expression (Expr));

         when N_Unchecked_Type_Conversion =>
            Check_Node (Expression (Expr));

         --  Checking should not be called directly on these nodes

         when N_Target_Name =>
            raise Program_Error;

         --  Unsupported constructs in SPARK

         when N_Delta_Aggregate =>
            Error_Msg_N ("unsupported construct in SPARK", Expr);

         --  Ignored constructs for pointer checking

         when N_Character_Literal
            | N_Null
            | N_Numeric_Or_String_Literal
            | N_Operator_Symbol
            | N_Raise_Expression
            | N_Raise_xxx_Error
         =>
            null;

         --  The following nodes are never generated in GNATprove mode

         when N_Expression_With_Actions
            | N_Reference
            | N_Unchecked_Expression
         =>
            raise Program_Error;

      end case;
   end Check_Expression;

   -------------------
   -- Check_Globals --
   -------------------

   procedure Check_Globals (N : Node_Id; Check_Mode : Checking_Mode) is
   begin
      if Nkind (N) = N_Empty then
         return;
      end if;

      declare
         pragma Assert
           (List_Length (Pragma_Argument_Associations (N)) = 1);

         PAA      : constant Node_Id :=
           First (Pragma_Argument_Associations (N));
         pragma Assert (Nkind (PAA) = N_Pragma_Argument_Association);

         Row      : Node_Id;
         The_Mode : Name_Id;
         RHS      : Node_Id;

         procedure Process (Mode   : Name_Id;
                            The_Global : Entity_Id);

         procedure Process (Mode   : Name_Id;
                            The_Global : Node_Id)
         is
            Ident_Elt : constant Entity_Id :=
              Unique_Entity (Entity (The_Global));

            Mode_Before : constant Checking_Mode := Current_Checking_Mode;

         begin
            if Ekind (Ident_Elt) = E_Abstract_State then
               return;
            end if;

            case Check_Mode is
               when Read =>
                  case Mode is
                     when Name_Input
                        | Name_Proof_In
                     =>
                        Check_Node (The_Global);

                     when Name_Output
                        | Name_In_Out
                     =>
                        null;

                     when others =>
                        raise Program_Error;

                  end case;

               when Observe =>
                  case Mode is
                     when Name_Input
                        | Name_Proof_In
                     =>
                        if not Is_Borrowed_In (Ident_Elt) then
                           --  Observed in

                           Current_Checking_Mode := Observe;
                           Check_Node (The_Global);
                        end if;

                     when others =>
                        null;

                  end case;

               when Borrow_Out =>

                  case Mode is
                     when Name_Output =>
                        --  Borrowed out
                        Current_Checking_Mode := Borrow_Out;
                        Check_Node (The_Global);

                     when others =>
                        null;

                  end case;

               when Move =>
                  case Mode is
                     when Name_Input
                        | Name_Proof_In
                     =>
                        if Is_Borrowed_In (Ident_Elt) then
                           --  Borrowed in

                           Current_Checking_Mode := Move;
                        else
                           --  Observed

                           return;
                        end if;

                     when Name_Output =>
                        return;

                     when Name_In_Out =>
                        --  Borrowed in out

                        Current_Checking_Mode := Move;

                     when others =>
                        raise Program_Error;
                  end case;

                  Check_Node (The_Global);
               when Assign =>
                  case Mode is
                     when Name_Input
                        | Name_Proof_In
                     =>
                        null;

                     when Name_Output
                        | Name_In_Out
                     =>
                        --  Borrowed out or in out

                        Process_Path (The_Global);

                     when others =>
                        raise Program_Error;
                  end case;

               when others =>
                  raise Program_Error;
            end case;

            Current_Checking_Mode := Mode_Before;
         end Process;

      begin
         if Nkind (Expression (PAA)) = N_Null then
            --  global => null
            --  No globals, nothing to do
            return;

         elsif Nkind_In (Expression (PAA), N_Identifier, N_Expanded_Name) then
            --  global => foo
            --  A single input
            Process (Name_Input, Expression (PAA));

         elsif Nkind (Expression (PAA)) = N_Aggregate
           and then Expressions (Expression (PAA)) /= No_List
         then
            --  global => (foo, bar)
            --  Inputs
            RHS := First (Expressions (Expression (PAA)));
            while Present (RHS) loop
               case Nkind (RHS) is
                  when N_Identifier
                     | N_Expanded_Name
                  =>
                     Process (Name_Input, RHS);

                  when N_Numeric_Or_String_Literal =>
                     Process (Name_Input, Original_Node (RHS));

                  when others =>
                     raise Program_Error;

               end case;
               RHS := Next (RHS);
            end loop;

         elsif Nkind (Expression (PAA)) = N_Aggregate
           and then Component_Associations (Expression (PAA)) /= No_List
         then
            --  global => (mode => foo,
            --             mode => (bar, baz))
            --  A mixture of things

            declare
               CA : constant List_Id :=
                 Component_Associations (Expression (PAA));
            begin
               Row := First (CA);
               while Present (Row) loop
                  pragma Assert (List_Length (Choices (Row)) = 1);
                  The_Mode := Chars (First (Choices (Row)));

                  RHS := Expression (Row);
                  case Nkind (RHS) is
                     when N_Aggregate =>
                        RHS := First (Expressions (RHS));
                        while Present (RHS) loop
                           case Nkind (RHS) is
                              when N_Numeric_Or_String_Literal =>
                                 Process (The_Mode, Original_Node (RHS));

                              when others =>
                                 Process (The_Mode, RHS);

                           end case;
                           RHS := Next (RHS);
                        end loop;

                     when N_Identifier
                        | N_Expanded_Name
                     =>
                        Process (The_Mode, RHS);

                     when N_Null =>
                        null;

                     when N_Numeric_Or_String_Literal =>
                        Process (The_Mode, Original_Node (RHS));

                     when others =>
                        raise Program_Error;

                  end case;

                  Row := Next (Row);
               end loop;
            end;

         else
            raise Program_Error;
         end if;
      end;
   end Check_Globals;

   ----------------
   -- Check_List --
   ----------------

   procedure Check_List (L : List_Id) is
      N : Node_Id;
   begin
      N := First (L);
      while Present (N) loop
         Check_Node (N);
         Next (N);
      end loop;
   end Check_List;

   --------------------------
   -- Check_Loop_Statement --
   --------------------------

   procedure Check_Loop_Statement (Loop_N : Node_Id) is

      --  Local Subprograms

      procedure Check_Is_Less_Restrictive_Env
        (Exiting_Env : Perm_Env;
         Entry_Env   : Perm_Env);
      --  This procedure checks that the Exiting_Env environment is less
      --  restrictive than the Entry_Env environment.

      procedure Check_Is_Less_Restrictive_Tree
        (New_Tree  : Perm_Tree_Access;
         Orig_Tree : Perm_Tree_Access;
         E         : Entity_Id);
      --  Auxiliary procedure to check that the tree New_Tree is less
      --  restrictive than the tree Orig_Tree for the entity E.

      procedure Perm_Error_Loop_Exit
        (E          : Entity_Id;
         Loop_Id    : Node_Id;
         Perm       : Perm_Kind;
         Found_Perm : Perm_Kind);
      --  A procedure that is called when the permissions found contradict
      --  the rules established by the RM at the exit of loops. This function
      --  is called with the entity, the node of the enclosing loop, the
      --  permission that was expected and the permission found, and issues
      --  an appropriate error message.

      -----------------------------------
      -- Check_Is_Less_Restrictive_Env --
      -----------------------------------

      procedure Check_Is_Less_Restrictive_Env
        (Exiting_Env : Perm_Env;
         Entry_Env   : Perm_Env)
      is
         Comp_Entry : Perm_Tree_Maps.Key_Option;
         Iter_Entry, Iter_Exit : Perm_Tree_Access;

      begin
         Comp_Entry := Get_First_Key (Entry_Env);
         while Comp_Entry.Present loop
            Iter_Entry := Get (Entry_Env, Comp_Entry.K);
            pragma Assert (Iter_Entry /= null);
            Iter_Exit := Get (Exiting_Env, Comp_Entry.K);
            pragma Assert (Iter_Exit /= null);
            Check_Is_Less_Restrictive_Tree
              (New_Tree  => Iter_Exit,
               Orig_Tree => Iter_Entry,
               E         => Comp_Entry.K);
            Comp_Entry := Get_Next_Key (Entry_Env);
         end loop;
      end Check_Is_Less_Restrictive_Env;

      ------------------------------------
      -- Check_Is_Less_Restrictive_Tree --
      ------------------------------------

      procedure Check_Is_Less_Restrictive_Tree
        (New_Tree  : Perm_Tree_Access;
         Orig_Tree : Perm_Tree_Access;
         E         : Entity_Id)
      is
         -----------------------
         -- Local Subprograms --
         -----------------------

         procedure Check_Is_Less_Restrictive_Tree_Than
           (Tree : Perm_Tree_Access;
            Perm : Perm_Kind;
            E    : Entity_Id);
         --  Auxiliary procedure to check that the tree N is less restrictive
         --  than the given permission P.

         procedure Check_Is_More_Restrictive_Tree_Than
           (Tree : Perm_Tree_Access;
            Perm : Perm_Kind;
            E    : Entity_Id);
         --  Auxiliary procedure to check that the tree N is more restrictive
         --  than the given permission P.

         -----------------------------------------
         -- Check_Is_Less_Restrictive_Tree_Than --
         -----------------------------------------

         procedure Check_Is_Less_Restrictive_Tree_Than
           (Tree : Perm_Tree_Access;
            Perm : Perm_Kind;
            E    : Entity_Id)
         is
         begin
            if not (Permission (Tree) >= Perm) then
               Perm_Error_Loop_Exit
                 (E, Loop_N, Permission (Tree), Perm);
            end if;

            case Kind (Tree) is
               when Entire_Object =>
                  if not (Children_Permission (Tree) >= Perm) then
                     Perm_Error_Loop_Exit
                       (E, Loop_N, Children_Permission (Tree), Perm);

                  end if;

               when Reference =>
                  Check_Is_Less_Restrictive_Tree_Than
                    (Get_All (Tree), Perm, E);

               when Array_Component =>
                  Check_Is_Less_Restrictive_Tree_Than
                    (Get_Elem (Tree), Perm, E);

               when Record_Component =>
                  declare
                     Comp : Perm_Tree_Access;
                  begin
                     Comp := Perm_Tree_Maps.Get_First (Component (Tree));
                     while Comp /= null loop
                        Check_Is_Less_Restrictive_Tree_Than (Comp, Perm, E);
                        Comp :=
                          Perm_Tree_Maps.Get_Next (Component (Tree));
                     end loop;

                     Check_Is_Less_Restrictive_Tree_Than
                       (Other_Components (Tree), Perm, E);
                  end;
            end case;
         end Check_Is_Less_Restrictive_Tree_Than;

         -----------------------------------------
         -- Check_Is_More_Restrictive_Tree_Than --
         -----------------------------------------

         procedure Check_Is_More_Restrictive_Tree_Than
           (Tree : Perm_Tree_Access;
            Perm : Perm_Kind;
            E    : Entity_Id)
         is
         begin
            if not (Perm >= Permission (Tree)) then
               Perm_Error_Loop_Exit
                 (E, Loop_N, Permission (Tree), Perm);
            end if;

            case Kind (Tree) is
               when Entire_Object =>
                  if not (Perm >= Children_Permission (Tree)) then
                     Perm_Error_Loop_Exit
                       (E, Loop_N, Children_Permission (Tree), Perm);
                  end if;

               when Reference =>
                  Check_Is_More_Restrictive_Tree_Than
                    (Get_All (Tree), Perm, E);

               when Array_Component =>
                  Check_Is_More_Restrictive_Tree_Than
                    (Get_Elem (Tree), Perm, E);

               when Record_Component =>
                  declare
                     Comp : Perm_Tree_Access;
                  begin
                     Comp := Perm_Tree_Maps.Get_First (Component (Tree));
                     while Comp /= null loop
                        Check_Is_More_Restrictive_Tree_Than (Comp, Perm, E);
                        Comp :=
                          Perm_Tree_Maps.Get_Next (Component (Tree));
                     end loop;

                     Check_Is_More_Restrictive_Tree_Than
                       (Other_Components (Tree), Perm, E);
                  end;
            end case;
         end Check_Is_More_Restrictive_Tree_Than;

      --  Start of processing for Check_Is_Less_Restrictive_Tree

      begin
         if not (Permission (New_Tree) <= Permission (Orig_Tree)) then
            Perm_Error_Loop_Exit
              (E          => E,
               Loop_Id    => Loop_N,
               Perm       => Permission (New_Tree),
               Found_Perm => Permission (Orig_Tree));
         end if;

         case Kind (New_Tree) is

            --  Potentially folded tree. We check the other tree Orig_Tree to
            --  check whether it is folded or not. If folded we just compare
            --  their Permission and Children_Permission, if not, then we
            --  look at the Children_Permission of the folded tree against
            --  the unfolded tree Orig_Tree.

            when Entire_Object =>
               case Kind (Orig_Tree) is
               when Entire_Object =>
                  if not (Children_Permission (New_Tree) <=
                          Children_Permission (Orig_Tree))
                  then
                     Perm_Error_Loop_Exit
                       (E, Loop_N,
                        Children_Permission (New_Tree),
                        Children_Permission (Orig_Tree));
                  end if;

               when Reference =>
                  Check_Is_More_Restrictive_Tree_Than
                    (Get_All (Orig_Tree), Children_Permission (New_Tree), E);

               when Array_Component =>
                  Check_Is_More_Restrictive_Tree_Than
                    (Get_Elem (Orig_Tree), Children_Permission (New_Tree), E);

               when Record_Component =>
                  declare
                     Comp : Perm_Tree_Access;
                  begin
                     Comp := Perm_Tree_Maps.Get_First
                       (Component (Orig_Tree));
                     while Comp /= null loop
                        Check_Is_More_Restrictive_Tree_Than
                          (Comp, Children_Permission (New_Tree), E);
                        Comp := Perm_Tree_Maps.Get_Next
                          (Component (Orig_Tree));
                     end loop;

                     Check_Is_More_Restrictive_Tree_Than
                       (Other_Components (Orig_Tree),
                        Children_Permission (New_Tree), E);
                  end;
               end case;

            when Reference =>
               case Kind (Orig_Tree) is
               when Entire_Object =>
                  Check_Is_Less_Restrictive_Tree_Than
                    (Get_All (New_Tree), Children_Permission (Orig_Tree), E);

               when Reference =>
                  Check_Is_Less_Restrictive_Tree
                    (Get_All (New_Tree), Get_All (Orig_Tree), E);

               when others =>
                  raise Program_Error;
               end case;

            when Array_Component =>
               case Kind (Orig_Tree) is
               when Entire_Object =>
                  Check_Is_Less_Restrictive_Tree_Than
                    (Get_Elem (New_Tree), Children_Permission (Orig_Tree), E);

               when Array_Component =>
                  Check_Is_Less_Restrictive_Tree
                    (Get_Elem (New_Tree), Get_Elem (Orig_Tree), E);

               when others =>
                  raise Program_Error;
               end case;

            when Record_Component =>
               declare
                  CompN : Perm_Tree_Access;
               begin
                  CompN :=
                    Perm_Tree_Maps.Get_First (Component (New_Tree));
                  case Kind (Orig_Tree) is
                  when Entire_Object =>
                     while CompN /= null loop
                        Check_Is_Less_Restrictive_Tree_Than
                          (CompN, Children_Permission (Orig_Tree), E);

                        CompN :=
                          Perm_Tree_Maps.Get_Next (Component (New_Tree));
                     end loop;

                     Check_Is_Less_Restrictive_Tree_Than
                       (Other_Components (New_Tree),
                        Children_Permission (Orig_Tree),
                        E);

                  when Record_Component =>
                     declare

                        KeyO : Perm_Tree_Maps.Key_Option;
                        CompO : Perm_Tree_Access;
                     begin
                        KeyO := Perm_Tree_Maps.Get_First_Key
                          (Component (Orig_Tree));
                        while KeyO.Present loop
                           pragma Assert (CompO /= null);

                           Check_Is_Less_Restrictive_Tree (CompN, CompO, E);

                           KeyO := Perm_Tree_Maps.Get_Next_Key
                             (Component (Orig_Tree));
                           CompN := Perm_Tree_Maps.Get
                             (Component (New_Tree), KeyO.K);
                           CompO := Perm_Tree_Maps.Get
                             (Component (Orig_Tree), KeyO.K);
                        end loop;

                        Check_Is_Less_Restrictive_Tree
                          (Other_Components (New_Tree),
                           Other_Components (Orig_Tree),
                           E);
                     end;

                  when others =>
                     raise Program_Error;
                  end case;
               end;
         end case;
      end Check_Is_Less_Restrictive_Tree;

      --------------------------
      -- Perm_Error_Loop_Exit --
      --------------------------

      procedure Perm_Error_Loop_Exit
        (E          : Entity_Id;
         Loop_Id    : Node_Id;
         Perm       : Perm_Kind;
         Found_Perm : Perm_Kind)
      is
      begin
         Error_Msg_Node_2 := Loop_Id;
         Error_Msg_N ("insufficient permission for & when exiting loop &", E);
         Perm_Mismatch (Exp_Perm => Perm,
                        Act_Perm => Found_Perm,
                        N        => Loop_Id);
      end Perm_Error_Loop_Exit;

      --  Local variables

      Loop_Name : constant Entity_Id := Entity (Identifier (Loop_N));
      Loop_Env  : constant Perm_Env_Access := new Perm_Env;

   begin
      --  Save environment prior to the loop

      Copy_Env (From => Current_Perm_Env, To => Loop_Env.all);

      --  Add saved environment to loop environment

      Set (Current_Loops_Envs, Loop_Name, Loop_Env);

      --  If the loop is not a plain-loop, then it may either never be entered,
      --  or it may be exited after a number of iterations. Hence add the
      --  current permission environment as the initial loop exit environment.
      --  Otherwise, the loop exit environment remains empty until it is
      --  populated by analyzing exit statements.

      if Present (Iteration_Scheme (Loop_N)) then
         declare
            Exit_Env  : constant Perm_Env_Access := new Perm_Env;
         begin
            Copy_Env (From => Current_Perm_Env, To => Exit_Env.all);
            Set (Current_Loops_Accumulators, Loop_Name, Exit_Env);
         end;
      end if;

      --  Analyze loop

      Check_Node (Iteration_Scheme (Loop_N));
      Check_List (Statements (Loop_N));

      --  Check that environment gets less restrictive at end of loop

      Check_Is_Less_Restrictive_Env
        (Exiting_Env => Current_Perm_Env,
         Entry_Env   => Loop_Env.all);

      --  Set environment to the one for exiting the loop

      declare
         Exit_Env : constant Perm_Env_Access :=
           Get (Current_Loops_Accumulators, Loop_Name);
      begin
         Free_Env (Current_Perm_Env);

         --  In the normal case, Exit_Env is not null and we use it. In the
         --  degraded case of a plain-loop without exit statements, Exit_Env is
         --  null, and we use the initial permission environment at the start
         --  of the loop to continue analysis. Any environment would be fine
         --  here, since the code after the loop is dead code, but this way we
         --  avoid spurious errors by having at least variables in scope inside
         --  the environment.

         if Exit_Env /= null then
            Copy_Env (From => Exit_Env.all, To => Current_Perm_Env);
         else
            Copy_Env (From => Loop_Env.all, To => Current_Perm_Env);
         end if;

         Free_Env (Loop_Env.all);
         Free_Env (Exit_Env.all);
      end;
   end Check_Loop_Statement;

   ----------------
   -- Check_Node --
   ----------------

   procedure Check_Node (N : Node_Id) is
      Mode_Before : constant Checking_Mode := Current_Checking_Mode;
   begin
      case Nkind (N) is
         when N_Declaration =>
            Check_Declaration (N);

         when N_Subexpr =>
            Check_Expression (N);

         when N_Subtype_Indication =>
            Check_Node (Constraint (N));

         when N_Body_Stub =>
            Check_Node (Get_Body_From_Stub (N));

         when N_Statement_Other_Than_Procedure_Call =>
            Check_Statement (N);

         when N_Package_Body =>
            Check_Package_Body (N);

         when N_Subprogram_Body
            | N_Entry_Body
            | N_Task_Body
         =>
            Check_Callable_Body (N);

         when N_Protected_Body =>
            Check_List (Declarations (N));

         when N_Package_Declaration =>
            declare
               Spec : constant Node_Id := Specification (N);
            begin
               Current_Checking_Mode := Read;
               Check_List (Visible_Declarations (Spec));
               Check_List (Private_Declarations (Spec));

               Return_Declarations (Visible_Declarations (Spec));
               Return_Declarations (Private_Declarations (Spec));
            end;

         when N_Iteration_Scheme =>
            Current_Checking_Mode := Read;
            Check_Node (Condition (N));
            Check_Node (Iterator_Specification (N));
            Check_Node (Loop_Parameter_Specification (N));

         when N_Case_Expression_Alternative =>
            Current_Checking_Mode := Read;
            Check_List (Discrete_Choices (N));
            Current_Checking_Mode := Mode_Before;
            Check_Node (Expression (N));

         when N_Case_Statement_Alternative =>
            Current_Checking_Mode := Read;
            Check_List (Discrete_Choices (N));
            Current_Checking_Mode := Mode_Before;
            Check_List (Statements (N));

         when N_Component_Association =>
            Check_Node (Expression (N));

         when N_Handled_Sequence_Of_Statements =>
            Check_List (Statements (N));

         when N_Parameter_Association =>
            Check_Node (Explicit_Actual_Parameter (N));

         when N_Range_Constraint =>
            Check_Node (Range_Expression (N));

         when N_Index_Or_Discriminant_Constraint =>
            Check_List (Constraints (N));

         --  Checking should not be called directly on these nodes

         when N_Abortable_Part
            | N_Accept_Alternative
            | N_Access_Definition
            | N_Access_Function_Definition
            | N_Access_Procedure_Definition
            | N_Access_To_Object_Definition
            | N_Aspect_Specification
            | N_Compilation_Unit
            | N_Compilation_Unit_Aux
            | N_Component_Clause
            | N_Component_Definition
            | N_Component_List
            | N_Constrained_Array_Definition
            | N_Contract
            | N_Decimal_Fixed_Point_Definition
            | N_Defining_Character_Literal
            | N_Defining_Identifier
            | N_Defining_Operator_Symbol
            | N_Defining_Program_Unit_Name
            | N_Delay_Alternative
            | N_Derived_Type_Definition
            | N_Designator
            | N_Discriminant_Association
            | N_Discriminant_Specification
            | N_Elsif_Part
            | N_Entry_Body_Formal_Part
            | N_Enumeration_Type_Definition
            | N_Entry_Call_Alternative
            | N_Entry_Index_Specification
            | N_Error
            | N_Exception_Handler
            | N_Floating_Point_Definition
            | N_Formal_Decimal_Fixed_Point_Definition
            | N_Formal_Derived_Type_Definition
            | N_Formal_Discrete_Type_Definition
            | N_Formal_Floating_Point_Definition
            | N_Formal_Incomplete_Type_Definition
            | N_Formal_Modular_Type_Definition
            | N_Formal_Ordinary_Fixed_Point_Definition
            | N_Formal_Private_Type_Definition
            | N_Formal_Signed_Integer_Type_Definition
            | N_Generic_Association
            | N_Mod_Clause
            | N_Modular_Type_Definition
            | N_Ordinary_Fixed_Point_Definition
            | N_Package_Specification
            | N_Parameter_Specification
            | N_Pragma_Argument_Association
            | N_Protected_Definition
            | N_Push_Pop_xxx_Label
            | N_Real_Range_Specification
            | N_Record_Definition
            | N_SCIL_Dispatch_Table_Tag_Init
            | N_SCIL_Dispatching_Call
            | N_SCIL_Membership_Test
            | N_Signed_Integer_Type_Definition
            | N_Subunit
            | N_Task_Definition
            | N_Terminate_Alternative
            | N_Triggering_Alternative
            | N_Unconstrained_Array_Definition
            | N_Unused_At_Start
            | N_Unused_At_End
            | N_Variant
            | N_Variant_Part
         =>
            raise Program_Error;

         --  Unsupported constructs in SPARK

         when N_Iterated_Component_Association =>
            Error_Msg_N ("unsupported construct in SPARK", N);

         --  Ignored constructs for pointer checking

         when N_Abstract_Subprogram_Declaration
            | N_At_Clause
            | N_Attribute_Definition_Clause
            | N_Delta_Constraint
            | N_Digits_Constraint
            | N_Empty
            | N_Enumeration_Representation_Clause
            | N_Exception_Declaration
            | N_Exception_Renaming_Declaration
            | N_Formal_Package_Declaration
            | N_Formal_Subprogram_Declaration
            | N_Freeze_Entity
            | N_Freeze_Generic_Entity
            | N_Function_Instantiation
            | N_Generic_Function_Renaming_Declaration
            | N_Generic_Package_Declaration
            | N_Generic_Package_Renaming_Declaration
            | N_Generic_Procedure_Renaming_Declaration
            | N_Generic_Subprogram_Declaration
            | N_Implicit_Label_Declaration
            | N_Itype_Reference
            | N_Label
            | N_Number_Declaration
            | N_Object_Renaming_Declaration
            | N_Others_Choice
            | N_Package_Instantiation
            | N_Package_Renaming_Declaration
            | N_Pragma
            | N_Procedure_Instantiation
            | N_Record_Representation_Clause
            | N_Subprogram_Declaration
            | N_Subprogram_Renaming_Declaration
            | N_Task_Type_Declaration
            | N_Use_Package_Clause
            | N_With_Clause
            | N_Use_Type_Clause
            | N_Validate_Unchecked_Conversion
         =>
            null;

         --  The following nodes are rewritten by semantic analysis

         when N_Single_Protected_Declaration
            | N_Single_Task_Declaration
         =>
            raise Program_Error;
      end case;

      Current_Checking_Mode := Mode_Before;
   end Check_Node;

   ------------------------
   -- Check_Package_Body --
   ------------------------

   procedure Check_Package_Body (Pack : Node_Id) is
      Saved_Env : Perm_Env;
      CorSp : Node_Id;

   begin
      if Present (SPARK_Pragma (Defining_Entity (Pack, False))) then
         if Get_SPARK_Mode_From_Annotation
           (SPARK_Pragma (Defining_Entity (Pack))) /= Opt.On
         then
            return;
         end if;
      else
         return;
      end if;

      CorSp := Parent (Corresponding_Spec (Pack));
      while Nkind (CorSp) /= N_Package_Specification loop
         CorSp := Parent (CorSp);
      end loop;

      Check_List (Visible_Declarations (CorSp));

      --  Save environment

      Copy_Env (Current_Perm_Env,
                Saved_Env);

      Check_List (Private_Declarations (CorSp));

      --  Set mode to Read, and then analyze declarations and statements

      Current_Checking_Mode := Read;

      Check_List (Declarations (Pack));
      Check_Node (Handled_Statement_Sequence (Pack));

      --  Check RW for every stateful variable (i.e. in declarations)

      Return_Declarations (Private_Declarations (CorSp));
      Return_Declarations (Visible_Declarations (CorSp));
      Return_Declarations (Declarations (Pack));

      --  Restore previous environment (i.e. delete every nonvisible
      --  declaration) from environment.

      Free_Env (Current_Perm_Env);
      Copy_Env (Saved_Env,
                Current_Perm_Env);
   end Check_Package_Body;

   -----------------
   -- Check_Param --
   -----------------

   procedure Check_Param (Formal : Entity_Id; Actual : Node_Id) is
      Mode : constant Entity_Kind := Ekind (Formal);
      Mode_Before : constant Checking_Mode := Current_Checking_Mode;

   begin
      case Current_Checking_Mode is
         when Read =>
            case Formal_Kind'(Mode) is
               when E_In_Parameter =>
                  if Is_Borrowed_In (Formal) then
                     --  Borrowed in

                     Current_Checking_Mode := Move;
                  else
                     --  Observed

                     return;
                  end if;

               when E_Out_Parameter =>
                  return;

               when E_In_Out_Parameter =>
                  --  Borrowed in out

                  Current_Checking_Mode := Move;

            end case;

            Check_Node (Actual);

         when Assign =>
            case Formal_Kind'(Mode) is
               when E_In_Parameter =>
                  null;

               when E_Out_Parameter
                  | E_In_Out_Parameter
               =>
                  --  Borrowed out or in out

                  Process_Path (Actual);

            end case;

         when others =>
            raise Program_Error;

      end case;
      Current_Checking_Mode := Mode_Before;
   end Check_Param;

   --------------------------
   -- Check_Param_Observes --
   --------------------------

   procedure Check_Param_Observes (Formal : Entity_Id; Actual : Node_Id) is
      Mode : constant Entity_Kind := Ekind (Formal);
      Mode_Before : constant Checking_Mode := Current_Checking_Mode;

   begin
      case Mode is
         when E_In_Parameter =>
            if not Is_Borrowed_In (Formal) then
               --  Observed in

               Current_Checking_Mode := Observe;
               Check_Node (Actual);
            end if;

         when others =>
            null;

      end case;

      Current_Checking_Mode := Mode_Before;
   end Check_Param_Observes;

   ----------------------
   -- Check_Param_Outs --
   ----------------------

   procedure Check_Param_Outs (Formal : Entity_Id; Actual : Node_Id) is
      Mode : constant Entity_Kind := Ekind (Formal);
      Mode_Before : constant Checking_Mode := Current_Checking_Mode;

   begin

      case Mode is
         when E_Out_Parameter =>
            --  Borrowed out
            Current_Checking_Mode := Borrow_Out;
            Check_Node (Actual);

         when others =>
            null;

      end case;

      Current_Checking_Mode := Mode_Before;
   end Check_Param_Outs;

   ----------------------
   -- Check_Param_Read --
   ----------------------

   procedure Check_Param_Read (Formal : Entity_Id; Actual : Node_Id) is
      Mode : constant Entity_Kind := Ekind (Formal);

   begin
      pragma Assert (Current_Checking_Mode = Read);

      case Formal_Kind'(Mode) is
         when E_In_Parameter =>
            Check_Node (Actual);

         when E_Out_Parameter
            | E_In_Out_Parameter
         =>
            null;

      end case;
   end Check_Param_Read;

   -------------------------
   -- Check_Safe_Pointers --
   -------------------------

   procedure Check_Safe_Pointers (N : Node_Id) is

      --  Local subprograms

      procedure Check_List (L : List_Id);
      --  Call the main analysis procedure on each element of the list

      procedure Initialize;
      --  Initialize global variables before starting the analysis of a body

      ----------------
      -- Check_List --
      ----------------

      procedure Check_List (L : List_Id) is
         N : Node_Id;
      begin
         N := First (L);
         while Present (N) loop
            Check_Safe_Pointers (N);
            Next (N);
         end loop;
      end Check_List;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize is
      begin
         Reset (Current_Loops_Envs);
         Reset (Current_Loops_Accumulators);
         Reset (Current_Perm_Env);
         Reset (Current_Initialization_Map);
      end Initialize;

      --  Local variables

      Prag : Node_Id;
      --  SPARK_Mode pragma in application

   --  Start of processing for Check_Safe_Pointers

   begin
      Initialize;

      case Nkind (N) is
         when N_Compilation_Unit =>
            Check_Safe_Pointers (Unit (N));

         when N_Package_Body
            | N_Package_Declaration
            | N_Subprogram_Body
         =>
            Prag := SPARK_Pragma (Defining_Entity (N));
            if Present (Prag) then
               if Get_SPARK_Mode_From_Annotation (Prag) = Opt.Off then
                  return;
               else
                  Check_Node (N);
               end if;

            elsif Nkind (N) = N_Package_Body then
               Check_List (Declarations (N));

            elsif Nkind (N) = N_Package_Declaration then
               Check_List (Private_Declarations (Specification (N)));
               Check_List (Visible_Declarations (Specification (N)));
            end if;

         when others =>
            null;
      end case;
   end Check_Safe_Pointers;

   ---------------------
   -- Check_Statement --
   ---------------------

   procedure Check_Statement (Stmt : Node_Id) is
      Mode_Before : constant Checking_Mode := Current_Checking_Mode;
   begin
      case N_Statement_Other_Than_Procedure_Call'(Nkind (Stmt)) is
         when N_Entry_Call_Statement =>
            Check_Call_Statement (Stmt);

         --  Move right-hand side first, and then assign left-hand side

         when N_Assignment_Statement =>
            if Is_Deep (Etype (Expression (Stmt))) then
               Current_Checking_Mode := Move;
            else
               Current_Checking_Mode := Read;
            end if;

            Check_Node (Expression (Stmt));
            Current_Checking_Mode := Assign;
            Check_Node (Name (Stmt));

         when N_Block_Statement =>
            declare
               Saved_Env : Perm_Env;

            begin
               --  Save environment

               Copy_Env (Current_Perm_Env,
                                 Saved_Env);

               --  Analyze declarations and Handled_Statement_Sequences

               Current_Checking_Mode := Read;
               Check_List (Declarations (Stmt));
               Check_Node (Handled_Statement_Sequence (Stmt));

               --  Restore environment

               Free_Env (Current_Perm_Env);
               Copy_Env (Saved_Env,
                                 Current_Perm_Env);
            end;

         when N_Case_Statement =>
            declare
               Saved_Env : Perm_Env;

               --  Accumulator for the different branches

               New_Env : Perm_Env;

               Elmt : Node_Id := First (Alternatives (Stmt));

            begin
               Current_Checking_Mode := Read;
               Check_Node (Expression (Stmt));
               Current_Checking_Mode := Mode_Before;

               --  Save environment

               Copy_Env (Current_Perm_Env,
                                 Saved_Env);

               --  Here we have the original env in saved, current with a fresh
               --  copy, and new aliased.

               --  First alternative

               Check_Node (Elmt);
               Next (Elmt);

               Copy_Env (Current_Perm_Env,
                                 New_Env);
               Free_Env (Current_Perm_Env);

               --  Other alternatives

               while Present (Elmt) loop
                  --  Restore environment

                  Copy_Env (Saved_Env,
                                    Current_Perm_Env);

                  Check_Node (Elmt);

                  --  Merge Current_Perm_Env into New_Env

                  Merge_Envs (New_Env,
                                      Current_Perm_Env);

                  Next (Elmt);
               end loop;

               --  CLEANUP
               Copy_Env (New_Env,
                                 Current_Perm_Env);

               Free_Env (New_Env);
               Free_Env (Saved_Env);
            end;

         when N_Delay_Relative_Statement =>
            Check_Node (Expression (Stmt));

         when N_Delay_Until_Statement =>
            Check_Node (Expression (Stmt));

         when N_Loop_Statement =>
            Check_Loop_Statement (Stmt);

         --  If deep type expression, then move, else read

         when N_Simple_Return_Statement =>
            case Nkind (Expression (Stmt)) is
               when N_Empty =>
                  declare
                     --  ??? This does not take into account the fact that
                     --  a simple return inside an extended return statement
                     --  applies to the extended return statement.
                     Subp : constant Entity_Id :=
                       Return_Applies_To (Return_Statement_Entity (Stmt));
                  begin
                     Return_Parameters (Subp);
                     Return_Globals (Subp);
                  end;

               when others =>
                  if Is_Deep (Etype (Expression (Stmt))) then
                     Current_Checking_Mode := Move;
                  elsif Is_Shallow (Etype (Expression (Stmt))) then
                     Current_Checking_Mode := Read;
                  else
                     raise Program_Error;
                  end if;

                  Check_Node (Expression (Stmt));
            end case;

         when N_Extended_Return_Statement =>
            Check_List (Return_Object_Declarations (Stmt));
            Check_Node (Handled_Statement_Sequence (Stmt));

            Return_Declarations (Return_Object_Declarations (Stmt));

            declare
               --  ??? This does not take into account the fact that a simple
               --  return inside an extended return statement applies to the
               --  extended return statement.
               Subp : constant Entity_Id :=
                 Return_Applies_To (Return_Statement_Entity (Stmt));
            begin
               Return_Parameters (Subp);
               Return_Globals (Subp);
            end;

         --  Merge the current_Perm_Env with the accumulator for the given loop

         when N_Exit_Statement =>
            declare
               Loop_Name : constant Entity_Id := Loop_Of_Exit (Stmt);

               Saved_Accumulator : constant Perm_Env_Access :=
                 Get (Current_Loops_Accumulators, Loop_Name);

               Environment_Copy : constant Perm_Env_Access :=
                 new Perm_Env;
            begin

               Copy_Env (Current_Perm_Env,
                                 Environment_Copy.all);

               if Saved_Accumulator = null then
                  Set (Current_Loops_Accumulators,
                       Loop_Name, Environment_Copy);
               else
                  Merge_Envs (Saved_Accumulator.all,
                                      Environment_Copy.all);
               end if;
            end;

         --  Copy environment, run on each branch, and then merge

         when N_If_Statement =>
            declare
               Saved_Env : Perm_Env;

               --  Accumulator for the different branches

               New_Env : Perm_Env;

            begin

               Check_Node (Condition (Stmt));

               --  Save environment

               Copy_Env (Current_Perm_Env,
                                 Saved_Env);

               --  Here we have the original env in saved, current with a fresh
               --  copy.

               --  THEN PART

               Check_List (Then_Statements (Stmt));

               Copy_Env (Current_Perm_Env,
                                 New_Env);

               Free_Env (Current_Perm_Env);

               --  Here the new_environment contains curr env after then block

               --  ELSIF part
               declare
                  Elmt : Node_Id;

               begin
                  Elmt := First (Elsif_Parts (Stmt));
                  while Present (Elmt) loop
                     --  Transfer into accumulator, and restore from save

                     Copy_Env (Saved_Env,
                                       Current_Perm_Env);

                     Check_Node (Condition (Elmt));
                     Check_List (Then_Statements (Stmt));

                     --  Merge Current_Perm_Env into New_Env

                     Merge_Envs (New_Env,
                                         Current_Perm_Env);

                     Next (Elmt);
                  end loop;
               end;

               --  ELSE part

               --  Restore environment before if

               Copy_Env (Saved_Env,
                                 Current_Perm_Env);

               --  Here new environment contains the environment after then and
               --  current the fresh copy of old one.

               Check_List (Else_Statements (Stmt));

               Merge_Envs (New_Env,
                                   Current_Perm_Env);

               --  CLEANUP

               Copy_Env (New_Env,
                                 Current_Perm_Env);

               Free_Env (New_Env);
               Free_Env (Saved_Env);
            end;

         --  Unsupported constructs in SPARK

         when N_Abort_Statement
            | N_Accept_Statement
            | N_Asynchronous_Select
            | N_Code_Statement
            | N_Conditional_Entry_Call
            | N_Goto_Statement
            | N_Requeue_Statement
            | N_Selective_Accept
            | N_Timed_Entry_Call
         =>
            Error_Msg_N ("unsupported construct in SPARK", Stmt);

         --  Ignored constructs for pointer checking

         when N_Null_Statement
            | N_Raise_Statement
         =>
            null;

         --  The following nodes are never generated in GNATprove mode

         when N_Compound_Statement
            | N_Free_Statement
         =>
            raise Program_Error;
      end case;
   end Check_Statement;

   --------------
   -- Get_Perm --
   --------------

   function Get_Perm (N : Node_Id) return Perm_Kind is
      Tree_Or_Perm : constant Perm_Or_Tree := Get_Perm_Or_Tree (N);

   begin
      case Tree_Or_Perm.R is
         when Folded =>
            return Tree_Or_Perm.Found_Permission;

         when Unfolded =>
            pragma Assert (Tree_Or_Perm.Tree_Access /= null);
            return Permission (Tree_Or_Perm.Tree_Access);

         --  We encoutered a function call, hence the memory area is fresh,
         --  which means that the association permission is RW.

         when Function_Call =>
            return Read_Write;

      end case;
   end Get_Perm;

   ----------------------
   -- Get_Perm_Or_Tree --
   ----------------------

   function Get_Perm_Or_Tree (N : Node_Id) return Perm_Or_Tree is
   begin
      case Nkind (N) is

         --  Base identifier. Normally those are the roots of the trees stored
         --  in the permission environment.

         when N_Defining_Identifier =>
            raise Program_Error;

         when N_Identifier
            | N_Expanded_Name
         =>
            declare
               P : constant Entity_Id := Entity (N);

               C : constant Perm_Tree_Access :=
                 Get (Current_Perm_Env, Unique_Entity (P));

            begin
               --  Setting the initialization map to True, so that this
               --  variable cannot be ignored anymore when looking at end
               --  of elaboration of package.

               Set (Current_Initialization_Map, Unique_Entity (P), True);

               if C = null then
                  --  No null possible here, there are no parents for the path.
                  --  This means we are using a global variable without adding
                  --  it in environment with a global aspect.

                  Illegal_Global_Usage (N);
               else
                  return (R => Unfolded, Tree_Access => C);
               end if;
            end;

         when N_Type_Conversion
            | N_Unchecked_Type_Conversion
            | N_Qualified_Expression
         =>
            return Get_Perm_Or_Tree (Expression (N));

         --  Happening when we try to get the permission of a variable that
         --  is a formal parameter. We get instead the defining identifier
         --  associated with the parameter (which is the one that has been
         --  stored for indexing).

         when N_Parameter_Specification =>
            return Get_Perm_Or_Tree (Defining_Identifier (N));

         --  We get the permission tree of its prefix, and then get either the
         --  subtree associated with that specific selection, or if we have a
         --  leaf that folds its children, we take the children's permission
         --  and return it using the discriminant Folded.

         when N_Selected_Component =>
            declare
               C : constant Perm_Or_Tree :=
                 Get_Perm_Or_Tree (Prefix (N));

            begin
               case C.R is
                  when Folded
                     | Function_Call
                  =>
                     return C;

                  when Unfolded =>
                     pragma Assert (C.Tree_Access /= null);

                     pragma Assert (Kind (C.Tree_Access) = Entire_Object
                                    or else
                                    Kind (C.Tree_Access) = Record_Component);

                     if Kind (C.Tree_Access) = Record_Component then
                        declare
                           Selected_Component : constant Entity_Id :=
                             Entity (Selector_Name (N));

                           Selected_C : constant Perm_Tree_Access :=
                             Perm_Tree_Maps.Get
                               (Component (C.Tree_Access), Selected_Component);

                        begin
                           if Selected_C = null then
                              return (R => Unfolded,
                                      Tree_Access =>
                                        Other_Components (C.Tree_Access));
                           else
                              return (R => Unfolded,
                                      Tree_Access => Selected_C);
                           end if;
                        end;
                     elsif Kind (C.Tree_Access) = Entire_Object then
                        return (R => Folded, Found_Permission =>
                                  Children_Permission (C.Tree_Access));
                     else
                        raise Program_Error;
                     end if;
               end case;
            end;

         --  We get the permission tree of its prefix, and then get either the
         --  subtree associated with that specific selection, or if we have a
         --  leaf that folds its children, we take the children's permission
         --  and return it using the discriminant Folded.

         when N_Indexed_Component
            | N_Slice
         =>
            declare
               C : constant Perm_Or_Tree :=
                 Get_Perm_Or_Tree (Prefix (N));

            begin
               case C.R is
                  when Folded
                     | Function_Call
                  =>
                     return C;

                  when Unfolded =>
                     pragma Assert (C.Tree_Access /= null);

                     pragma Assert (Kind (C.Tree_Access) = Entire_Object
                                    or else
                                    Kind (C.Tree_Access) = Array_Component);

                     if Kind (C.Tree_Access) = Array_Component then
                        pragma Assert (Get_Elem (C.Tree_Access) /= null);

                        return (R => Unfolded,
                                Tree_Access => Get_Elem (C.Tree_Access));
                     elsif Kind (C.Tree_Access) = Entire_Object then
                        return (R => Folded, Found_Permission =>
                                  Children_Permission (C.Tree_Access));
                     else
                        raise Program_Error;
                     end if;
               end case;
            end;

         --  We get the permission tree of its prefix, and then get either the
         --  subtree associated with that specific selection, or if we have a
         --  leaf that folds its children, we take the children's permission
         --  and return it using the discriminant Folded.

         when N_Explicit_Dereference =>
            declare
               C : constant Perm_Or_Tree :=
                 Get_Perm_Or_Tree (Prefix (N));

            begin
               case C.R is
                  when Folded
                     | Function_Call
                  =>
                     return C;

                  when Unfolded =>
                     pragma Assert (C.Tree_Access /= null);

                     pragma Assert (Kind (C.Tree_Access) = Entire_Object
                                    or else
                                    Kind (C.Tree_Access) = Reference);

                     if Kind (C.Tree_Access) = Reference then
                        if Get_All (C.Tree_Access) = null then
                           --  Hash_Table_Error
                           raise Program_Error;
                        else
                           return
                             (R => Unfolded,
                              Tree_Access => Get_All (C.Tree_Access));
                        end if;
                     elsif Kind (C.Tree_Access) = Entire_Object then
                        return (R => Folded, Found_Permission =>
                                  Children_Permission (C.Tree_Access));
                     else
                        raise Program_Error;
                     end if;
               end case;
            end;

         --  The name contains a function call, hence the given path is always
         --  new. We do not have to check for anything.

         when N_Function_Call =>
            return (R => Function_Call);

         when others =>
            raise Program_Error;
      end case;
   end Get_Perm_Or_Tree;

   -------------------
   -- Get_Perm_Tree --
   -------------------

   function Get_Perm_Tree
     (N : Node_Id)
       return Perm_Tree_Access
   is
   begin
      case Nkind (N) is

         --  Base identifier. Normally those are the roots of the trees stored
         --  in the permission environment.

         when N_Defining_Identifier =>
            raise Program_Error;

         when N_Identifier
            | N_Expanded_Name
         =>
            declare
               P : constant Node_Id := Entity (N);

               C : constant Perm_Tree_Access :=
                 Get (Current_Perm_Env, Unique_Entity (P));

            begin
               --  Setting the initialization map to True, so that this
               --  variable cannot be ignored anymore when looking at end
               --  of elaboration of package.

               Set (Current_Initialization_Map, Unique_Entity (P), True);

               if C = null then
                  --  No null possible here, there are no parents for the path.
                  --  This means we are using a global variable without adding
                  --  it in environment with a global aspect.

                  Illegal_Global_Usage (N);
               else
                  return C;
               end if;
            end;

         when N_Type_Conversion
            | N_Unchecked_Type_Conversion
            | N_Qualified_Expression
         =>
            return Get_Perm_Tree (Expression (N));

         when N_Parameter_Specification =>
            return Get_Perm_Tree (Defining_Identifier (N));

         --  We get the permission tree of its prefix, and then get either the
         --  subtree associated with that specific selection, or if we have a
         --  leaf that folds its children, we unroll it in one step.

         when N_Selected_Component =>
            declare
               C : constant Perm_Tree_Access :=
                 Get_Perm_Tree (Prefix (N));

            begin
               if C = null then
                  --  If null then it means we went through a function call

                  return null;
               end if;

               pragma Assert (Kind (C) = Entire_Object
                              or else Kind (C) = Record_Component);

               if Kind (C) = Record_Component then
                  --  The tree is unfolded. We just return the subtree.

                  declare
                     Selected_Component : constant Entity_Id :=
                       Entity (Selector_Name (N));
                     Selected_C : constant Perm_Tree_Access :=
                       Perm_Tree_Maps.Get
                         (Component (C), Selected_Component);

                  begin
                     if Selected_C = null then
                        return Other_Components (C);
                     end if;

                     return Selected_C;
                  end;
               elsif Kind (C) = Entire_Object then
                  declare
                     --  Expand the tree. Replace the node with
                     --  Record_Component.

                     Elem : Node_Id;

                     --  Create the unrolled nodes

                     Son : Perm_Tree_Access;

                     Child_Perm : constant Perm_Kind :=
                       Children_Permission (C);

                  begin

                     --  We change the current node from Entire_Object to
                     --  Record_Component with same permission and an empty
                     --  hash table as component list.

                     C.all.Tree :=
                       (Kind             => Record_Component,
                        Is_Node_Deep     => Is_Node_Deep (C),
                        Permission       => Permission (C),
                        Component        => Perm_Tree_Maps.Nil,
                        Other_Components =>
                           new Perm_Tree_Wrapper'
                          (Tree =>
                               (Kind                => Entire_Object,
                                --  Is_Node_Deep is true, to be conservative
                                Is_Node_Deep        => True,
                                Permission          => Child_Perm,
                                Children_Permission => Child_Perm)
                          )
                       );

                     --  We fill the hash table with all sons of the record,
                     --  with basic Entire_Objects nodes.
                     Elem := First_Component_Or_Discriminant
                       (Etype (Prefix (N)));

                     while Present (Elem) loop
                        Son := new Perm_Tree_Wrapper'
                          (Tree =>
                             (Kind                => Entire_Object,
                              Is_Node_Deep        => Is_Deep (Etype (Elem)),
                              Permission          => Child_Perm,
                              Children_Permission => Child_Perm));

                        Perm_Tree_Maps.Set
                          (C.all.Tree.Component, Elem, Son);

                        Next_Component_Or_Discriminant (Elem);
                     end loop;

                     --  we return the tree to the sons, so that the recursion
                     --  can continue.

                     declare
                        Selected_Component : constant Entity_Id :=
                          Entity (Selector_Name (N));

                        Selected_C : constant Perm_Tree_Access :=
                          Perm_Tree_Maps.Get
                            (Component (C), Selected_Component);

                     begin
                        pragma Assert (Selected_C /= null);

                        return Selected_C;
                     end;

                  end;
               else
                  raise Program_Error;
               end if;
            end;

         --  We set the permission tree of its prefix, and then we extract from
         --  the returned pointer the subtree. If folded, we unroll the tree at
         --  one step.

         when N_Indexed_Component
            | N_Slice
         =>
            declare
               C : constant Perm_Tree_Access :=
                 Get_Perm_Tree (Prefix (N));

            begin
               if C = null then
                  --  If null then we went through a function call

                  return null;
               end if;

               pragma Assert (Kind (C) = Entire_Object
                              or else Kind (C) = Array_Component);

               if Kind (C) = Array_Component then
                  --  The tree is unfolded. We just return the elem subtree

                  pragma Assert (Get_Elem (C) = null);

                  return Get_Elem (C);
               elsif Kind (C) = Entire_Object then
                  declare
                     --  Expand the tree. Replace node with Array_Component.

                     Son : Perm_Tree_Access;

                  begin
                     Son := new Perm_Tree_Wrapper'
                       (Tree =>
                          (Kind                => Entire_Object,
                           Is_Node_Deep        => Is_Node_Deep (C),
                           Permission          => Children_Permission (C),
                           Children_Permission => Children_Permission (C)));

                     --  We change the current node from Entire_Object
                     --  to Array_Component with same permission and the
                     --  previously defined son.

                     C.all.Tree := (Kind         => Array_Component,
                                    Is_Node_Deep => Is_Node_Deep (C),
                                    Permission   => Permission (C),
                                    Get_Elem     => Son);

                     return Get_Elem (C);
                  end;
               else
                  raise Program_Error;
               end if;
            end;

         --  We get the permission tree of its prefix, and then get either the
         --  subtree associated with that specific selection, or if we have a
         --  leaf that folds its children, we unroll the tree.

         when N_Explicit_Dereference =>
            declare
               C : Perm_Tree_Access;

            begin
               C := Get_Perm_Tree (Prefix (N));

               if C = null then
                  --  If null, we went through a function call

                  return null;
               end if;

               pragma Assert (Kind (C) = Entire_Object
                              or else Kind (C) = Reference);

               if Kind (C) = Reference then
                  --  The tree is unfolded. We return the elem subtree

                  if Get_All (C) = null then
                     --  Hash_Table_Error
                     raise Program_Error;
                  end if;

                  return Get_All (C);
               elsif Kind (C) = Entire_Object then
                  declare
                     --  Expand the tree. Replace the node with Reference.

                     Son : Perm_Tree_Access;

                  begin
                     Son := new Perm_Tree_Wrapper'
                       (Tree =>
                          (Kind                => Entire_Object,
                           Is_Node_Deep        => Is_Deep (Etype (N)),
                           Permission          => Children_Permission (C),
                           Children_Permission => Children_Permission (C)));

                     --  We change the current node from Entire_Object to
                     --  Reference with same permission and the previous son.

                     pragma Assert (Is_Node_Deep (C));

                     C.all.Tree := (Kind         => Reference,
                                    Is_Node_Deep => Is_Node_Deep (C),
                                    Permission   => Permission (C),
                                    Get_All      => Son);

                     return Get_All (C);
                  end;
               else
                  raise Program_Error;
               end if;
            end;

         --  No permission tree for function calls

         when N_Function_Call =>
            return null;

         when others =>
            raise Program_Error;
      end case;
   end Get_Perm_Tree;

   ---------
   -- Glb --
   ---------

   function Glb (P1, P2 : Perm_Kind) return Perm_Kind
   is
   begin
      case P1 is
         when No_Access =>
            return No_Access;

         when Read_Only =>
            case P2 is
               when No_Access
                  | Write_Only
               =>
                  return No_Access;

               when Read_Perm =>
                  return Read_Only;
            end case;

         when Write_Only =>
            case P2 is
               when No_Access
                  | Read_Only
               =>
                  return No_Access;

               when Write_Perm =>
                  return Write_Only;
            end case;

         when Read_Write =>
            return P2;
      end case;
   end Glb;

   ---------------
   -- Has_Alias --
   ---------------

   function Has_Alias
     (N : Node_Id)
       return Boolean
   is
      function Has_Alias_Deep (Typ : Entity_Id) return Boolean;
      function Has_Alias_Deep (Typ : Entity_Id) return Boolean
      is
         Comp : Node_Id;
      begin

         if Is_Array_Type (Typ)
           and then Has_Aliased_Components (Typ)
         then
            return True;

            --  Note: Has_Aliased_Components applies only to arrays

         elsif Is_Record_Type (Typ) then
            --  It is possible to have an aliased discriminant, so they must be
            --  checked along with normal components.

            Comp := First_Component_Or_Discriminant (Typ);
            while Present (Comp) loop
               if Is_Aliased (Comp)
                 or else Is_Aliased (Etype (Comp))
               then
                  return True;
               end if;

               if Has_Alias_Deep (Etype (Comp)) then
                  return True;
               end if;

               Next_Component_Or_Discriminant (Comp);
            end loop;
            return False;
         else
            return Is_Aliased (Typ);
         end if;
      end Has_Alias_Deep;

   begin
      case Nkind (N) is

         when N_Identifier
            | N_Expanded_Name
         =>
            return Has_Alias_Deep (Etype (N));

         when N_Defining_Identifier =>
            return Has_Alias_Deep (Etype (N));

         when N_Type_Conversion
            | N_Unchecked_Type_Conversion
            | N_Qualified_Expression
         =>
            return Has_Alias (Expression (N));

         when N_Parameter_Specification =>
            return Has_Alias (Defining_Identifier (N));

         when N_Selected_Component =>
            case Nkind (Selector_Name (N)) is
               when N_Identifier =>
                  if Is_Aliased (Entity (Selector_Name (N))) then
                     return True;
                  end if;

               when others => null;

            end case;

            return Has_Alias (Prefix (N));

         when N_Indexed_Component
            | N_Slice
         =>
            return Has_Alias (Prefix (N));

         when N_Explicit_Dereference =>
            return True;

         when N_Function_Call =>
            return False;

         when N_Attribute_Reference =>
            if Is_Deep (Etype (Prefix (N))) then
               raise Program_Error;
            end if;
            return False;

         when others =>
            return False;
      end case;
   end Has_Alias;

   -------------------------
   -- Has_Array_Component --
   -------------------------

   function Has_Array_Component (N : Node_Id) return Boolean is
   begin
      case Nkind (N) is
         --  Base identifier. There is no array component here.

         when N_Identifier
            | N_Expanded_Name
            | N_Defining_Identifier
         =>
            return False;

         --  We check if the expression inside the conversion has an array
         --  component.

         when N_Type_Conversion
            | N_Unchecked_Type_Conversion
            | N_Qualified_Expression
         =>
            return Has_Array_Component (Expression (N));

         --  We check if the prefix has an array component

         when N_Selected_Component =>
            return Has_Array_Component (Prefix (N));

         --  We found the array component, return True

         when N_Indexed_Component
            | N_Slice
         =>
            return True;

         --  We check if the prefix has an array component

         when N_Explicit_Dereference =>
            return Has_Array_Component (Prefix (N));

         when N_Function_Call =>
            return False;

         when others =>
            raise Program_Error;
      end case;
   end Has_Array_Component;

   ----------------------------
   -- Has_Function_Component --
   ----------------------------

   function Has_Function_Component (N : Node_Id) return Boolean is
   begin
      case Nkind (N) is
         --  Base identifier. There is no function component here.

         when N_Identifier
            | N_Expanded_Name
            | N_Defining_Identifier
         =>
            return False;

         --  We check if the expression inside the conversion has a function
         --  component.

         when N_Type_Conversion
            | N_Unchecked_Type_Conversion
            | N_Qualified_Expression
         =>
            return Has_Function_Component (Expression (N));

         --  We check if the prefix has a function component

         when N_Selected_Component =>
            return Has_Function_Component (Prefix (N));

         --  We check if the prefix has a function component

         when N_Indexed_Component
            | N_Slice
         =>
            return Has_Function_Component (Prefix (N));

         --  We check if the prefix has a function component

         when N_Explicit_Dereference =>
            return Has_Function_Component (Prefix (N));

         --  We found the function component, return True

         when N_Function_Call =>
            return True;

         when others =>
            raise Program_Error;

      end case;
   end Has_Function_Component;

   --------
   -- Hp --
   --------

   procedure Hp (P : Perm_Env) is
      Elem : Perm_Tree_Maps.Key_Option;

   begin
      Elem := Get_First_Key (P);
      while Elem.Present loop
         Print_Node_Briefly (Elem.K);
         Elem := Get_Next_Key (P);
      end loop;
   end Hp;

   --------------------------
   -- Illegal_Global_Usage --
   --------------------------

   procedure Illegal_Global_Usage (N : Node_Or_Entity_Id)  is
   begin
      Error_Msg_NE ("cannot use global variable & of deep type", N, N);
      Error_Msg_N ("\without prior declaration in a Global aspect", N);

      Errout.Finalize (Last_Call => True);
      Errout.Output_Messages;
      Exit_Program (E_Errors);
   end Illegal_Global_Usage;

   --------------------
   -- Is_Borrowed_In --
   --------------------

   function Is_Borrowed_In (E : Entity_Id) return Boolean is
   begin
      return Is_Access_Type (Etype (E))
        and then not Is_Access_Constant (Etype (E));
   end Is_Borrowed_In;

   -------------
   -- Is_Deep --
   -------------

   function Is_Deep (E : Entity_Id) return Boolean is
      function Is_Private_Entity_Mode_Off (E : Entity_Id) return Boolean;

      function Is_Private_Entity_Mode_Off (E : Entity_Id) return Boolean is
         Decl : Node_Id;
         Pack_Decl : Node_Id;

      begin
         if Is_Itype (E) then
            Decl := Associated_Node_For_Itype (E);
         else
            Decl := Parent (E);
         end if;

         Pack_Decl := Parent (Parent (Decl));

         if Nkind (Pack_Decl) /= N_Package_Declaration then
            return False;
         end if;

         return
           Present (SPARK_Aux_Pragma (Defining_Entity (Pack_Decl)))
           and then Get_SPARK_Mode_From_Annotation
             (SPARK_Aux_Pragma (Defining_Entity (Pack_Decl))) = Off;
      end Is_Private_Entity_Mode_Off;
   begin
      pragma Assert (Is_Type (E));

      case Ekind (E) is
         when Scalar_Kind =>
            return False;

         when Access_Kind =>
            return True;

         --  Just check the depth of its component type

         when E_Array_Type
            | E_Array_Subtype
         =>
            return Is_Deep (Component_Type (E));

         when E_String_Literal_Subtype =>
            return False;

         --  Per RM 8.11 for class-wide types

         when E_Class_Wide_Subtype
            | E_Class_Wide_Type
         =>
            return True;

         --  ??? What about hidden components

         when E_Record_Type
            | E_Record_Subtype
            =>
            declare
               Elmt : Entity_Id;

            begin
               Elmt := First_Component_Or_Discriminant (E);
               while Present (Elmt) loop
                  if Is_Deep (Etype (Elmt)) then
                     return True;
                  else
                     Next_Component_Or_Discriminant (Elmt);
                  end if;
               end loop;

               return False;
            end;

         when Private_Kind =>
            if Is_Private_Entity_Mode_Off (E) then
               return False;
            else
               if Present (Full_View (E)) then
                  return Is_Deep (Full_View (E));
               else
                  return True;
               end if;
            end if;

         when E_Incomplete_Type =>
            return True;

         when E_Incomplete_Subtype =>
            return True;

         --  No problem with synchronized types

         when E_Protected_Type
            | E_Protected_Subtype
            | E_Task_Subtype
            | E_Task_Type
          =>
            return False;

         when E_Exception_Type =>
            return False;

         when others =>
            raise Program_Error;
      end case;
   end Is_Deep;

   ----------------
   -- Is_Shallow --
   ----------------

   function Is_Shallow (E : Entity_Id) return Boolean is
   begin
      pragma Assert (Is_Type (E));
      return not Is_Deep (E);
   end Is_Shallow;

   ------------------
   -- Loop_Of_Exit --
   ------------------

   function Loop_Of_Exit (N : Node_Id) return Entity_Id is
      Nam : Node_Id := Name (N);
      Stmt : Node_Id := N;
   begin
      if No (Nam) then
         while Present (Stmt) loop
            Stmt := Parent (Stmt);
            if Nkind (Stmt) = N_Loop_Statement then
               Nam := Identifier (Stmt);
               exit;
            end if;
         end loop;
      end if;
      return Entity (Nam);
   end Loop_Of_Exit;
   ---------
   -- Lub --
   ---------

   function Lub (P1, P2 : Perm_Kind) return Perm_Kind
   is
   begin
      case P1 is
         when No_Access =>
            return P2;

         when Read_Only =>
            case P2 is
               when No_Access
                  | Read_Only
               =>
                  return Read_Only;

               when Write_Perm =>
                  return Read_Write;
            end case;

         when Write_Only =>
            case P2 is
               when No_Access
                  | Write_Only
               =>
                  return Write_Only;

               when Read_Perm =>
                  return Read_Write;
            end case;

         when Read_Write =>
            return Read_Write;
      end case;
   end Lub;

   ----------------
   -- Merge_Envs --
   ----------------

   procedure Merge_Envs
     (Target : in out Perm_Env;
      Source : in out Perm_Env)
   is
      procedure Merge_Trees
        (Target : Perm_Tree_Access;
         Source : Perm_Tree_Access);

      procedure Merge_Trees
        (Target : Perm_Tree_Access;
         Source : Perm_Tree_Access)
      is
         procedure Apply_Glb_Tree
           (A : Perm_Tree_Access;
            P : Perm_Kind);

         procedure Apply_Glb_Tree
           (A : Perm_Tree_Access;
            P : Perm_Kind)
         is
         begin
            A.all.Tree.Permission := Glb (Permission (A), P);

            case Kind (A) is
               when Entire_Object =>
                  A.all.Tree.Children_Permission :=
                    Glb (Children_Permission (A), P);

               when Reference =>
                  Apply_Glb_Tree (Get_All (A), P);

               when Array_Component =>
                  Apply_Glb_Tree (Get_Elem (A), P);

               when Record_Component =>
                  declare
                     Comp : Perm_Tree_Access;
                  begin
                     Comp := Perm_Tree_Maps.Get_First (Component (A));
                     while Comp /= null loop
                        Apply_Glb_Tree (Comp, P);
                        Comp := Perm_Tree_Maps.Get_Next (Component (A));
                     end loop;

                     Apply_Glb_Tree (Other_Components (A), P);
                  end;
            end case;
         end Apply_Glb_Tree;

         Perm : constant Perm_Kind :=
           Glb (Permission (Target), Permission (Source));

      begin
         pragma Assert (Is_Node_Deep (Target) = Is_Node_Deep (Source));
         Target.all.Tree.Permission := Perm;

         case Kind (Target) is
            when Entire_Object =>
               declare
                  Child_Perm : constant Perm_Kind :=
                    Children_Permission (Target);

               begin
                  case Kind (Source) is
                  when Entire_Object =>
                     Target.all.Tree.Children_Permission :=
                       Glb (Child_Perm, Children_Permission (Source));

                  when Reference =>
                     Copy_Tree (Source, Target);
                     Target.all.Tree.Permission := Perm;
                     Apply_Glb_Tree (Get_All (Target), Child_Perm);

                  when Array_Component =>
                     Copy_Tree (Source, Target);
                     Target.all.Tree.Permission := Perm;
                     Apply_Glb_Tree (Get_Elem (Target), Child_Perm);

                  when Record_Component =>
                     Copy_Tree (Source, Target);
                     Target.all.Tree.Permission := Perm;
                     declare
                        Comp : Perm_Tree_Access;

                     begin
                        Comp :=
                          Perm_Tree_Maps.Get_First (Component (Target));
                        while Comp /= null loop
                           --  Apply glb tree on every component subtree

                           Apply_Glb_Tree (Comp, Child_Perm);
                           Comp := Perm_Tree_Maps.Get_Next
                             (Component (Target));
                        end loop;
                     end;
                     Apply_Glb_Tree (Other_Components (Target), Child_Perm);

                  end case;
               end;
            when Reference =>
               case Kind (Source) is
               when Entire_Object =>
                  Apply_Glb_Tree (Get_All (Target),
                                  Children_Permission (Source));

               when Reference =>
                  Merge_Trees (Get_All (Target), Get_All (Source));

               when others =>
                  raise Program_Error;

               end case;

            when Array_Component =>
               case Kind (Source) is
               when Entire_Object =>
                  Apply_Glb_Tree (Get_Elem (Target),
                                  Children_Permission (Source));

               when Array_Component =>
                  Merge_Trees (Get_Elem (Target), Get_Elem (Source));

               when others =>
                  raise Program_Error;

               end case;

            when Record_Component =>
               case Kind (Source) is
               when Entire_Object =>
                  declare
                     Child_Perm : constant Perm_Kind :=
                       Children_Permission (Source);

                     Comp : Perm_Tree_Access;

                  begin
                     Comp := Perm_Tree_Maps.Get_First
                       (Component (Target));
                     while Comp /= null loop
                        --  Apply glb tree on every component subtree

                        Apply_Glb_Tree (Comp, Child_Perm);
                        Comp :=
                          Perm_Tree_Maps.Get_Next (Component (Target));
                     end loop;
                     Apply_Glb_Tree (Other_Components (Target), Child_Perm);
                  end;

               when Record_Component =>
                  declare
                     Key_Source : Perm_Tree_Maps.Key_Option;
                     CompTarget : Perm_Tree_Access;
                     CompSource : Perm_Tree_Access;

                  begin
                     Key_Source := Perm_Tree_Maps.Get_First_Key
                       (Component (Source));

                     while Key_Source.Present loop
                        CompSource := Perm_Tree_Maps.Get
                          (Component (Source), Key_Source.K);
                        CompTarget := Perm_Tree_Maps.Get
                          (Component (Target), Key_Source.K);

                        pragma Assert (CompSource /= null);
                        Merge_Trees (CompTarget, CompSource);

                        Key_Source := Perm_Tree_Maps.Get_Next_Key
                          (Component (Source));
                     end loop;

                     Merge_Trees (Other_Components (Target),
                                  Other_Components (Source));
                  end;

               when others =>
                  raise Program_Error;

               end case;
         end case;
      end Merge_Trees;

      CompTarget : Perm_Tree_Access;
      CompSource : Perm_Tree_Access;
      KeyTarget : Perm_Tree_Maps.Key_Option;

   begin
      KeyTarget := Get_First_Key (Target);
      --  Iterate over every tree of the environment in the target, and merge
      --  it with the source if there is such a similar one that exists. If
      --  there is none, then skip.
      while KeyTarget.Present loop

         CompSource := Get (Source, KeyTarget.K);
         CompTarget := Get (Target, KeyTarget.K);

         pragma Assert (CompTarget /= null);

         if CompSource /= null then
            Merge_Trees (CompTarget, CompSource);
            Remove (Source, KeyTarget.K);
         end if;

         KeyTarget := Get_Next_Key (Target);
      end loop;

      --  Iterate over every tree of the environment of the source. And merge
      --  again. If there is not any tree of the target then just copy the tree
      --  from source to target.
      declare
         KeySource : Perm_Tree_Maps.Key_Option;
      begin
         KeySource := Get_First_Key (Source);
         while KeySource.Present loop

            CompSource := Get (Source, KeySource.K);
            CompTarget := Get (Target, KeySource.K);

            if CompTarget = null then
               CompTarget := new Perm_Tree_Wrapper'(CompSource.all);
               Copy_Tree (CompSource, CompTarget);
               Set (Target, KeySource.K, CompTarget);
            else
               Merge_Trees (CompTarget, CompSource);
            end if;

            KeySource := Get_Next_Key (Source);
         end loop;
      end;

      Free_Env (Source);
   end Merge_Envs;

   ----------------
   -- Perm_Error --
   ----------------

   procedure Perm_Error
     (N : Node_Id;
      Perm : Perm_Kind;
      Found_Perm : Perm_Kind)
   is
      procedure Set_Root_Object
        (Path  : Node_Id;
         Obj   : out Entity_Id;
         Deref : out Boolean);
      --  Set the root object Obj, and whether the path contains a dereference,
      --  from a path Path.

      ---------------------
      -- Set_Root_Object --
      ---------------------

      procedure Set_Root_Object
        (Path  : Node_Id;
         Obj   : out Entity_Id;
         Deref : out Boolean)
      is
      begin
         case Nkind (Path) is
            when N_Identifier
               | N_Expanded_Name
            =>
               Obj := Entity (Path);
               Deref := False;

            when N_Type_Conversion
               | N_Unchecked_Type_Conversion
               | N_Qualified_Expression
            =>
               Set_Root_Object (Expression (Path), Obj, Deref);

            when N_Indexed_Component
               | N_Selected_Component
               | N_Slice
            =>
               Set_Root_Object (Prefix (Path), Obj, Deref);

            when N_Explicit_Dereference =>
               Set_Root_Object (Prefix (Path), Obj, Deref);
               Deref := True;

            when others =>
               raise Program_Error;
         end case;
      end Set_Root_Object;

      --  Local variables

      Root : Entity_Id;
      Is_Deref : Boolean;

   --  Start of processing for Perm_Error

   begin
      Set_Root_Object (N, Root, Is_Deref);

      if Is_Deref then
         Error_Msg_NE
           ("insufficient permission on dereference from &", N, Root);
      else
         Error_Msg_NE ("insufficient permission for &", N, Root);
      end if;

      Perm_Mismatch (Perm, Found_Perm, N);
   end Perm_Error;

   -------------------------------
   -- Perm_Error_Subprogram_End --
   -------------------------------

   procedure Perm_Error_Subprogram_End
     (E          : Entity_Id;
      Subp       : Entity_Id;
      Perm       : Perm_Kind;
      Found_Perm : Perm_Kind)
   is
   begin
      Error_Msg_Node_2 := Subp;
      Error_Msg_NE ("insufficient permission for & when returning from &",
                    Subp, E);
      Perm_Mismatch (Perm, Found_Perm, Subp);
   end Perm_Error_Subprogram_End;

   ------------------
   -- Process_Path --
   ------------------

   procedure Process_Path (N : Node_Id) is
      Root : constant Entity_Id := Get_Enclosing_Object (N);
   begin
      --  We ignore if yielding to synchronized

      if Present (Root)
        and then Is_Synchronized_Object (Root)
      then
         return;
      end if;

      --  We ignore shallow unaliased. They are checked in flow analysis,
      --  allowing backward compatibility.

      if not Has_Alias (N)
        and then Is_Shallow (Etype (N))
      then
         return;
      end if;

      declare
         Perm_N : constant Perm_Kind := Get_Perm (N);

      begin

         case Current_Checking_Mode is
            --  Check permission R, do nothing

            when Read =>
               if Perm_N not in Read_Perm then
                  Perm_Error (N, Read_Only, Perm_N);
               end if;

            --  If shallow type no need for RW, only R

            when Move =>
               if Is_Shallow (Etype (N)) then
                  if Perm_N not in Read_Perm then
                     Perm_Error (N, Read_Only, Perm_N);
                  end if;
               else
                  --  Check permission RW if deep

                  if Perm_N /= Read_Write then
                     Perm_Error (N, Read_Write, Perm_N);
                  end if;

                  declare
                     --  Set permission to W to the path and any of its prefix

                     Tree : constant Perm_Tree_Access :=
                       Set_Perm_Prefixes_Move (N, Move);

                  begin
                     if Tree = null then
                        --  We went through a function call, no permission to
                        --  modify.

                        return;
                     end if;

                     --  Set permissions to
                     --  No for any extension with more .all
                     --  W for any deep extension with same number of .all
                     --  RW for any shallow extension with same number of .all

                     Set_Perm_Extensions_Move (Tree, Etype (N));
                  end;
               end if;

            --  Check permission RW

            when Super_Move =>
               if Perm_N /= Read_Write then
                  Perm_Error (N, Read_Write, Perm_N);
               end if;

               declare
                  --  Set permission to No to the path and any of its prefix up
                  --  to the first .all and then W.

                  Tree : constant Perm_Tree_Access :=
                    Set_Perm_Prefixes_Move (N, Super_Move);

               begin
                  if Tree = null then
                     --  We went through a function call, no permission to
                     --  modify.

                     return;
                  end if;

                  --  Set permissions to No on any strict extension of the path

                  Set_Perm_Extensions (Tree, No_Access);
               end;

            --  Check permission W

            when Assign =>
               if Perm_N not in Write_Perm then
                  Perm_Error (N, Write_Only, Perm_N);
               end if;

               --  If the tree has an array component, then the permissions do
               --  not get modified by the assignment.

               if Has_Array_Component (N) then
                  return;
               end if;

               --  Same if has function component

               if Has_Function_Component (N) then
                  return;
               end if;

               declare
                  --  Get the permission tree for the path

                  Tree : constant Perm_Tree_Access :=
                    Get_Perm_Tree (N);

                  Dummy : Perm_Tree_Access;

               begin
                  if Tree = null then
                     --  We went through a function call, no permission to
                     --  modify.

                     return;
                  end if;

                  --  Set permission RW for it and all of its extensions

                  Tree.all.Tree.Permission := Read_Write;

                  Set_Perm_Extensions (Tree, Read_Write);

                  --  Normalize the permission tree

                  Dummy := Set_Perm_Prefixes_Assign (N);
               end;

            --  Check permission W

            when Borrow_Out =>
               if Perm_N not in Write_Perm then
                  Perm_Error (N, Write_Only, Perm_N);
               end if;

               declare
                  --  Set permission to No to the path and any of its prefixes

                  Tree : constant Perm_Tree_Access :=
                    Set_Perm_Prefixes_Borrow_Out (N);

               begin
                  if Tree = null then
                     --  We went through a function call, no permission to
                     --  modify.

                     return;
                  end if;

                  --  Set permissions to No on any strict extension of the path

                  Set_Perm_Extensions (Tree, No_Access);
               end;

            when Observe =>
               if Perm_N not in Read_Perm then
                  Perm_Error (N, Read_Only, Perm_N);
               end if;

               if Is_By_Copy_Type (Etype (N)) then
                  return;
               end if;

               declare
                  --  Set permission to No on the path and any of its prefixes

                  Tree : constant Perm_Tree_Access :=
                    Set_Perm_Prefixes_Observe (N);

               begin
                  if Tree = null then
                     --  We went through a function call, no permission to
                     --  modify.

                     return;
                  end if;

                  --  Set permissions to No on any strict extension of the path

                  Set_Perm_Extensions (Tree, Read_Only);
               end;
         end case;
      end;
   end Process_Path;

   -------------------------
   -- Return_Declarations --
   -------------------------

   procedure Return_Declarations (L : List_Id) is

      procedure Return_Declaration (Decl : Node_Id);
      --  Check correct permissions for every declared object

      ------------------------
      -- Return_Declaration --
      ------------------------

      procedure Return_Declaration (Decl : Node_Id) is
      begin
         if Nkind (Decl) = N_Object_Declaration then
            --  Check RW for object declared, unless the object has never been
            --  initialized.

            if Get (Current_Initialization_Map,
                    Unique_Entity (Defining_Identifier (Decl))) = False
            then
               return;
            end if;

            --  We ignore shallow unaliased. They are checked in flow analysis,
            --  allowing backward compatibility.

            if not Has_Alias (Defining_Identifier (Decl))
              and then Is_Shallow (Etype (Defining_Identifier (Decl)))
            then
               return;
            end if;

            declare
               Elem : constant Perm_Tree_Access :=
                 Get (Current_Perm_Env,
                      Unique_Entity (Defining_Identifier (Decl)));

            begin
               if Elem = null then
                  --  Here we are on a declaration. Hence it should have been
                  --  added in the environment when analyzing this node with
                  --  mode Read. Hence it is not possible to find a null
                  --  pointer here.

                  --  Hash_Table_Error
                  raise Program_Error;
               end if;

               if Permission (Elem) /= Read_Write then
                  Perm_Error (Decl, Read_Write, Permission (Elem));
               end if;
            end;
         end if;
      end Return_Declaration;

      --  Local Variables

      N : Node_Id;

   --  Start of processing for Return_Declarations

   begin
      N := First (L);
      while Present (N) loop
         Return_Declaration (N);
         Next (N);
      end loop;
   end Return_Declarations;

   --------------------
   -- Return_Globals --
   --------------------

   procedure Return_Globals (Subp : Entity_Id) is

      procedure Return_Globals_From_List
        (First_Item : Node_Id;
         Kind       : Formal_Kind);
      --  Return global items from the list starting at Item

      procedure Return_Globals_Of_Mode (Global_Mode : Name_Id);
      --  Return global items for the mode Global_Mode

      ------------------------------
      -- Return_Globals_From_List --
      ------------------------------

      procedure Return_Globals_From_List
        (First_Item : Node_Id;
         Kind       : Formal_Kind)
      is
         Item : Node_Id := First_Item;
         E    : Entity_Id;

      begin
         while Present (Item) loop
            E := Entity (Item);

            --  Ignore abstract states, which play no role in pointer aliasing

            if Ekind (E) = E_Abstract_State then
               null;
            else
               Return_Parameter_Or_Global (E, Kind, Subp);
            end if;
            Next_Global (Item);
         end loop;
      end Return_Globals_From_List;

      ----------------------------
      -- Return_Globals_Of_Mode --
      ----------------------------

      procedure Return_Globals_Of_Mode (Global_Mode : Name_Id) is
         Kind : Formal_Kind;

      begin
         case Global_Mode is
            when Name_Input | Name_Proof_In =>
               Kind := E_In_Parameter;
            when Name_Output =>
               Kind := E_Out_Parameter;
            when Name_In_Out =>
               Kind := E_In_Out_Parameter;
            when others =>
               raise Program_Error;
         end case;

         --  Return both global items from Global and Refined_Global pragmas

         Return_Globals_From_List (First_Global (Subp, Global_Mode), Kind);
         Return_Globals_From_List
           (First_Global (Subp, Global_Mode, Refined => True), Kind);
      end Return_Globals_Of_Mode;

   --  Start of processing for Return_Globals

   begin
      Return_Globals_Of_Mode (Name_Proof_In);
      Return_Globals_Of_Mode (Name_Input);
      Return_Globals_Of_Mode (Name_Output);
      Return_Globals_Of_Mode (Name_In_Out);
   end Return_Globals;

   --------------------------------
   -- Return_Parameter_Or_Global --
   --------------------------------

   procedure Return_Parameter_Or_Global
     (Id   : Entity_Id;
      Mode : Formal_Kind;
      Subp : Entity_Id)
   is
      Elem : constant Perm_Tree_Access := Get (Current_Perm_Env, Id);
      pragma Assert (Elem /= null);

   begin
      --  Shallow unaliased parameters and globals cannot introduce pointer
      --  aliasing.

      if not Has_Alias (Id) and then Is_Shallow (Etype (Id)) then
         null;

      --  Observed IN parameters and globals need not return a permission to
      --  the caller.

      elsif Mode = E_In_Parameter and then not Is_Borrowed_In (Id) then
         null;

      --  All other parameters and globals should return with mode RW to the
      --  caller.

      else
         if Permission (Elem) /= Read_Write then
            Perm_Error_Subprogram_End
              (E          => Id,
               Subp       => Subp,
               Perm       => Read_Write,
               Found_Perm => Permission (Elem));
         end if;
      end if;
   end Return_Parameter_Or_Global;

   -----------------------
   -- Return_Parameters --
   -----------------------

   procedure Return_Parameters (Subp : Entity_Id) is
      Formal : Entity_Id;

   begin
      Formal := First_Formal (Subp);
      while Present (Formal) loop
         Return_Parameter_Or_Global (Formal, Ekind (Formal), Subp);
         Next_Formal (Formal);
      end loop;
   end Return_Parameters;

   -------------------------
   -- Set_Perm_Extensions --
   -------------------------

   procedure Set_Perm_Extensions
     (T : Perm_Tree_Access;
      P : Perm_Kind)
   is
      procedure Free_Perm_Tree_Children (T : Perm_Tree_Access);

      procedure Free_Perm_Tree_Children (T : Perm_Tree_Access)
      is
      begin
         case Kind (T) is
            when Entire_Object =>
               null;

            when Reference =>
               Free_Perm_Tree (T.all.Tree.Get_All);

            when Array_Component =>
               Free_Perm_Tree (T.all.Tree.Get_Elem);

            --  Free every Component subtree

            when Record_Component =>
               declare
                  Comp : Perm_Tree_Access;

               begin
                  Comp := Perm_Tree_Maps.Get_First (Component (T));
                  while Comp /= null loop
                     Free_Perm_Tree (Comp);
                     Comp := Perm_Tree_Maps.Get_Next (Component (T));
                  end loop;

                  Free_Perm_Tree (T.all.Tree.Other_Components);
               end;
         end case;
      end Free_Perm_Tree_Children;

      Son : constant Perm_Tree :=
        Perm_Tree'
          (Kind                => Entire_Object,
           Is_Node_Deep        => Is_Node_Deep (T),
           Permission          => Permission (T),
           Children_Permission => P);

   begin
      Free_Perm_Tree_Children (T);
      T.all.Tree := Son;
   end Set_Perm_Extensions;

   ------------------------------
   -- Set_Perm_Extensions_Move --
   ------------------------------

   procedure Set_Perm_Extensions_Move
     (T : Perm_Tree_Access;
      E : Entity_Id)
   is
   begin
      if not Is_Node_Deep (T) then
         --  We are a shallow extension with same number of .all

         Set_Perm_Extensions (T, Read_Write);
         return;
      end if;

      --  We are a deep extension here (or the moved deep path)

      T.all.Tree.Permission := Write_Only;

      case T.all.Tree.Kind is
         --  Unroll the tree depending on the type

         when Entire_Object =>
            case Ekind (E) is
               when Scalar_Kind
                  | E_String_Literal_Subtype
               =>
                  Set_Perm_Extensions (T, No_Access);

               --  No need to unroll here, directly put sons to No_Access

               when Access_Kind =>
                  if Ekind (E) in Access_Subprogram_Kind then
                     null;
                  else
                     Set_Perm_Extensions (T, No_Access);
                  end if;

               --  No unrolling done, too complicated

               when E_Class_Wide_Subtype
                  | E_Class_Wide_Type
                  | E_Incomplete_Type
                  | E_Incomplete_Subtype
                  | E_Exception_Type
                  | E_Task_Type
                  | E_Task_Subtype
               =>
                  Set_Perm_Extensions (T, No_Access);

               --  Expand the tree. Replace the node with Array component.

               when E_Array_Type
                  | E_Array_Subtype =>
                  declare
                     Son : Perm_Tree_Access;

                  begin
                     Son := new Perm_Tree_Wrapper'
                       (Tree =>
                          (Kind                => Entire_Object,
                           Is_Node_Deep        => Is_Node_Deep (T),
                           Permission          => Read_Write,
                           Children_Permission => Read_Write));

                     Set_Perm_Extensions_Move (Son, Component_Type (E));

                     --  We change the current node from Entire_Object to
                     --  Reference with Write_Only and the previous son.

                     pragma Assert (Is_Node_Deep (T));

                     T.all.Tree := (Kind         => Array_Component,
                                    Is_Node_Deep => Is_Node_Deep (T),
                                    Permission   => Write_Only,
                                    Get_Elem     => Son);
                  end;

               --  Unroll, and set permission extensions with component type

               when E_Record_Type
                  | E_Record_Subtype
                  | E_Record_Type_With_Private
                  | E_Record_Subtype_With_Private
                  | E_Protected_Type
                  | E_Protected_Subtype
               =>
                  declare
                     --  Expand the tree. Replace the node with
                     --  Record_Component.

                     Elem : Node_Id;

                     Son : Perm_Tree_Access;

                  begin
                     --  We change the current node from Entire_Object to
                     --  Record_Component with same permission and an empty
                     --  hash table as component list.

                     pragma Assert (Is_Node_Deep (T));

                     T.all.Tree :=
                       (Kind             => Record_Component,
                        Is_Node_Deep     => Is_Node_Deep (T),
                        Permission       => Write_Only,
                        Component        => Perm_Tree_Maps.Nil,
                        Other_Components =>
                           new Perm_Tree_Wrapper'
                          (Tree =>
                               (Kind                => Entire_Object,
                                Is_Node_Deep        => True,
                                Permission          => Read_Write,
                                Children_Permission => Read_Write)
                          )
                       );

                     --  We fill the hash table with all sons of the record,
                     --  with basic Entire_Objects nodes.
                     Elem := First_Component_Or_Discriminant (E);
                     while Present (Elem) loop
                        Son := new Perm_Tree_Wrapper'
                          (Tree =>
                             (Kind                => Entire_Object,
                              Is_Node_Deep        => Is_Deep (Etype (Elem)),
                              Permission          => Read_Write,
                              Children_Permission => Read_Write));

                        Set_Perm_Extensions_Move (Son, Etype (Elem));

                        Perm_Tree_Maps.Set
                          (T.all.Tree.Component, Elem, Son);

                        Next_Component_Or_Discriminant (Elem);
                     end loop;
                  end;

               when E_Private_Type
                  | E_Private_Subtype
                  | E_Limited_Private_Type
                  | E_Limited_Private_Subtype
               =>
                  Set_Perm_Extensions_Move (T, Underlying_Type (E));

               when others =>
                  raise Program_Error;
            end case;

         when Reference =>
            --  Now the son does not have the same number of .all
            Set_Perm_Extensions (T, No_Access);

         when Array_Component =>
            Set_Perm_Extensions_Move (Get_Elem (T), Component_Type (E));

         when Record_Component =>
            declare
               Comp : Perm_Tree_Access;
               It : Node_Id;

            begin
               It := First_Component_Or_Discriminant (E);
               while It /= Empty loop
                  Comp := Perm_Tree_Maps.Get (Component (T), It);
                  pragma Assert (Comp /= null);
                  Set_Perm_Extensions_Move (Comp, It);
                  It := Next_Component_Or_Discriminant (E);
               end loop;

               Set_Perm_Extensions (Other_Components (T), No_Access);
            end;
      end case;
   end Set_Perm_Extensions_Move;

   ------------------------------
   -- Set_Perm_Prefixes_Assign --
   ------------------------------

   function Set_Perm_Prefixes_Assign
     (N : Node_Id)
       return Perm_Tree_Access
   is
      C : constant Perm_Tree_Access := Get_Perm_Tree (N);

   begin
      pragma Assert (Current_Checking_Mode = Assign);

      --  The function should not be called if has_function_component

      pragma Assert (C /= null);

      case Kind (C) is
         when Entire_Object =>
            pragma Assert (Children_Permission (C) = Read_Write);
            C.all.Tree.Permission := Read_Write;

         when Reference =>
            pragma Assert (Get_All (C) /= null);

            C.all.Tree.Permission :=
              Lub (Permission (C), Permission (Get_All (C)));

         when Array_Component =>
            pragma Assert (C.all.Tree.Get_Elem /= null);

            --  Given that it is not possible to know which element has been
            --  assigned, then the permissions do not get changed in case of
            --  Array_Component.

            null;

         when Record_Component =>
            declare
               Perm : Perm_Kind := Read_Write;

               Comp : Perm_Tree_Access;

            begin
               --  We take the Glb of all the descendants, and then update the
               --  permission of the node with it.
               Comp := Perm_Tree_Maps.Get_First (Component (C));
               while Comp /= null loop
                  Perm := Glb (Perm, Permission (Comp));
                  Comp := Perm_Tree_Maps.Get_Next (Component (C));
               end loop;

               Perm := Glb (Perm, Permission (Other_Components (C)));

               C.all.Tree.Permission := Lub (Permission (C), Perm);
            end;
      end case;

      case Nkind (N) is
         --  Base identifier. End recursion here.

         when N_Identifier
            | N_Expanded_Name
            | N_Defining_Identifier
         =>
            return null;

         when N_Type_Conversion
            | N_Unchecked_Type_Conversion
            | N_Qualified_Expression
         =>
            return Set_Perm_Prefixes_Assign (Expression (N));

         when N_Parameter_Specification =>
            raise Program_Error;

         --  Continue recursion on prefix

         when N_Selected_Component =>
            return Set_Perm_Prefixes_Assign (Prefix (N));

         --  Continue recursion on prefix

         when N_Indexed_Component
            | N_Slice
         =>
            return Set_Perm_Prefixes_Assign (Prefix (N));

         --  Continue recursion on prefix

         when N_Explicit_Dereference =>
            return Set_Perm_Prefixes_Assign (Prefix (N));

         when N_Function_Call =>
            raise Program_Error;

         when others =>
            raise Program_Error;

      end case;
   end Set_Perm_Prefixes_Assign;

   ----------------------------------
   -- Set_Perm_Prefixes_Borrow_Out --
   ----------------------------------

   function Set_Perm_Prefixes_Borrow_Out
     (N : Node_Id)
       return Perm_Tree_Access
   is
   begin
      pragma Assert (Current_Checking_Mode = Borrow_Out);

      case Nkind (N) is
         --  Base identifier. Set permission to No.

         when N_Identifier
            | N_Expanded_Name
         =>
            declare
               P : constant Node_Id := Entity (N);

               C : constant Perm_Tree_Access :=
                 Get (Current_Perm_Env, Unique_Entity (P));

               pragma Assert (C /= null);

            begin
               --  Setting the initialization map to True, so that this
               --  variable cannot be ignored anymore when looking at end
               --  of elaboration of package.

               Set (Current_Initialization_Map, Unique_Entity (P), True);

               C.all.Tree.Permission := No_Access;
               return C;
            end;

         when N_Type_Conversion
            | N_Unchecked_Type_Conversion
            | N_Qualified_Expression
         =>
            return Set_Perm_Prefixes_Borrow_Out (Expression (N));

         when N_Parameter_Specification
            | N_Defining_Identifier
         =>
            raise Program_Error;

            --  We set the permission tree of its prefix, and then we extract
            --  our subtree from the returned pointer and assign an adequate
            --  permission to it, if unfolded. If folded, we unroll the tree
            --  in one step.

         when N_Selected_Component =>
            declare
               C : constant Perm_Tree_Access :=
                 Set_Perm_Prefixes_Borrow_Out (Prefix (N));

            begin
               if C = null then
                  --  We went through a function call, do nothing

                  return null;
               end if;

               --  The permission of the returned node should be No

               pragma Assert (Permission (C) = No_Access);

               pragma Assert (Kind (C) = Entire_Object
                              or else Kind (C) = Record_Component);

               if Kind (C) = Record_Component then
                  --  The tree is unfolded. We just modify the permission and
                  --  return the record subtree.

                  declare
                     Selected_Component : constant Entity_Id :=
                       Entity (Selector_Name (N));

                     Selected_C : Perm_Tree_Access :=
                       Perm_Tree_Maps.Get
                         (Component (C), Selected_Component);

                  begin
                     if Selected_C = null then
                        Selected_C := Other_Components (C);
                     end if;

                     pragma Assert (Selected_C /= null);

                     Selected_C.all.Tree.Permission := No_Access;

                     return Selected_C;
                  end;
               elsif Kind (C) = Entire_Object then
                  declare
                     --  Expand the tree. Replace the node with
                     --  Record_Component.

                     Elem : Node_Id;

                     --  Create an empty hash table

                     Hashtbl : Perm_Tree_Maps.Instance;

                     --  We create the unrolled nodes, that will all have same
                     --  permission than parent.

                     Son : Perm_Tree_Access;

                     ChildrenPerm : constant Perm_Kind :=
                       Children_Permission (C);

                  begin
                     --  We change the current node from Entire_Object to
                     --  Record_Component with same permission and an empty
                     --  hash table as component list.

                     C.all.Tree :=
                       (Kind         => Record_Component,
                        Is_Node_Deep => Is_Node_Deep (C),
                        Permission   => Permission (C),
                        Component    => Hashtbl,
                        Other_Components =>
                           new Perm_Tree_Wrapper'
                          (Tree =>
                               (Kind                => Entire_Object,
                                Is_Node_Deep        => True,
                                Permission          => ChildrenPerm,
                                Children_Permission => ChildrenPerm)
                          ));

                     --  We fill the hash table with all sons of the record,
                     --  with basic Entire_Objects nodes.
                     Elem := First_Component_Or_Discriminant
                       (Etype (Prefix (N)));

                     while Present (Elem) loop
                        Son := new Perm_Tree_Wrapper'
                          (Tree =>
                             (Kind                => Entire_Object,
                              Is_Node_Deep        => Is_Deep (Etype (Elem)),
                              Permission          => ChildrenPerm,
                              Children_Permission => ChildrenPerm));

                        Perm_Tree_Maps.Set
                          (C.all.Tree.Component, Elem, Son);

                        Next_Component_Or_Discriminant (Elem);
                     end loop;

                     --  Now we set the right field to No_Access, and then we
                     --  return the tree to the sons, so that the recursion can
                     --  continue.

                     declare
                        Selected_Component : constant Entity_Id :=
                          Entity (Selector_Name (N));

                        Selected_C : Perm_Tree_Access :=
                          Perm_Tree_Maps.Get
                            (Component (C), Selected_Component);

                     begin
                        if Selected_C = null then
                           Selected_C := Other_Components (C);
                        end if;

                        pragma Assert (Selected_C /= null);

                        Selected_C.all.Tree.Permission := No_Access;

                        return Selected_C;
                     end;
                  end;
               else
                  raise Program_Error;
               end if;
            end;

            --  We set the permission tree of its prefix, and then we extract
            --  from the returned pointer the subtree and assign an adequate
            --  permission to it, if unfolded. If folded, we unroll the tree in
            --  one step.

         when N_Indexed_Component
            | N_Slice
         =>
            declare
               C : constant Perm_Tree_Access :=
                 Set_Perm_Prefixes_Borrow_Out (Prefix (N));

            begin
               if C = null then
                  --  We went through a function call, do nothing

                  return null;
               end if;

               --  The permission of the returned node should be either W
               --  (because the recursive call sets <= Write_Only) or No
               --  (if another path has been moved with 'Access).

               pragma Assert (Permission (C) = No_Access);

               pragma Assert (Kind (C) = Entire_Object
                              or else Kind (C) = Array_Component);

               if Kind (C) = Array_Component then
                  --  The tree is unfolded. We just modify the permission and
                  --  return the elem subtree.

                  pragma Assert (Get_Elem (C) /= null);

                  C.all.Tree.Get_Elem.all.Tree.Permission := No_Access;

                  return Get_Elem (C);
               elsif Kind (C) = Entire_Object then
                  declare
                     --  Expand the tree. Replace node with Array_Component.

                     Son : Perm_Tree_Access;

                  begin
                     Son := new Perm_Tree_Wrapper'
                       (Tree =>
                          (Kind                => Entire_Object,
                           Is_Node_Deep        => Is_Node_Deep (C),
                           Permission          => No_Access,
                           Children_Permission => Children_Permission (C)));

                     --  We change the current node from Entire_Object
                     --  to Array_Component with same permission and the
                     --  previously defined son.

                     C.all.Tree := (Kind         => Array_Component,
                                    Is_Node_Deep => Is_Node_Deep (C),
                                    Permission   => No_Access,
                                    Get_Elem     => Son);

                     return Get_Elem (C);
                  end;
               else
                  raise Program_Error;
               end if;
            end;

            --  We set the permission tree of its prefix, and then we extract
            --  from the returned pointer the subtree and assign an adequate
            --  permission to it, if unfolded. If folded, we unroll the tree
            --  at one step.

         when N_Explicit_Dereference =>
            declare
               C : constant Perm_Tree_Access :=
                 Set_Perm_Prefixes_Borrow_Out (Prefix (N));

            begin
               if C = null then
                  --  We went through a function call. Do nothing.

                  return null;
               end if;

               --  The permission of the returned node should be No

               pragma Assert (Permission (C) = No_Access);
               pragma Assert (Kind (C) = Entire_Object
                              or else Kind (C) = Reference);

               if Kind (C) = Reference then
                  --  The tree is unfolded. We just modify the permission and
                  --  return the elem subtree.

                  pragma Assert (Get_All (C) /= null);

                  C.all.Tree.Get_All.all.Tree.Permission := No_Access;

                  return Get_All (C);
               elsif Kind (C) = Entire_Object then
                  declare
                     --  Expand the tree. Replace the node with Reference.

                     Son : Perm_Tree_Access;

                  begin
                     Son := new Perm_Tree_Wrapper'
                       (Tree =>
                          (Kind                => Entire_Object,
                           Is_Node_Deep        => Is_Deep (Etype (N)),
                           Permission          => No_Access,
                           Children_Permission => Children_Permission (C)));

                     --  We change the current node from Entire_Object to
                     --  Reference with No_Access and the previous son.

                     pragma Assert (Is_Node_Deep (C));

                     C.all.Tree := (Kind         => Reference,
                                    Is_Node_Deep => Is_Node_Deep (C),
                                    Permission   => No_Access,
                                    Get_All      => Son);

                     return Get_All (C);
                  end;
               else
                  raise Program_Error;
               end if;
            end;

         when N_Function_Call =>
            return null;

         when others =>
            raise Program_Error;
      end case;
   end Set_Perm_Prefixes_Borrow_Out;

   ----------------------------
   -- Set_Perm_Prefixes_Move --
   ----------------------------

   function Set_Perm_Prefixes_Move
     (N : Node_Id; Mode : Checking_Mode)
       return Perm_Tree_Access
   is
   begin
      case Nkind (N) is
         --  Base identifier. Set permission to W or No depending on Mode.

         when N_Identifier
            | N_Expanded_Name
         =>
            declare
               P : constant Node_Id := Entity (N);

               C : constant Perm_Tree_Access :=
                 Get (Current_Perm_Env, Unique_Entity (P));

            begin
               --  The base tree can be RW (first move from this base path) or
               --  W (already some extensions values moved), or even No_Access
               --  (extensions moved with 'Access). But it cannot be Read_Only
               --  (we get an error).

               if Permission (C) = Read_Only then
                  raise Unrecoverable_Error;
               end if;

               --  Setting the initialization map to True, so that this
               --  variable cannot be ignored anymore when looking at end
               --  of elaboration of package.

               Set (Current_Initialization_Map, Unique_Entity (P), True);

               if C = null then
                  --  No null possible here, there are no parents for the path.
                  --  This means we are using a global variable without adding
                  --  it in environment with a global aspect.

                  Illegal_Global_Usage (N);
               end if;

               if Mode = Super_Move then
                  C.all.Tree.Permission := No_Access;
               else
                  C.all.Tree.Permission := Glb (Write_Only, Permission (C));
               end if;

               return C;
            end;

         when N_Type_Conversion
            | N_Unchecked_Type_Conversion
            | N_Qualified_Expression
         =>
            return Set_Perm_Prefixes_Move (Expression (N), Mode);

         when N_Parameter_Specification
            | N_Defining_Identifier
         =>
            raise Program_Error;

            --  We set the permission tree of its prefix, and then we extract
            --  from the returned pointer our subtree and assign an adequate
            --  permission to it, if unfolded. If folded, we unroll the tree
            --  at one step.

         when N_Selected_Component =>
            declare
               C : constant Perm_Tree_Access :=
                 Set_Perm_Prefixes_Move (Prefix (N), Mode);

            begin
               if C = null then
                  --  We went through a function call, do nothing

                  return null;
               end if;

               --  The permission of the returned node should be either W
               --  (because the recursive call sets <= Write_Only) or No
               --  (if another path has been moved with 'Access).

               pragma Assert (Permission (C) = No_Access
                              or else Permission (C) = Write_Only);

               if Mode = Super_Move then
                  --  The permission of the returned node should be No (thanks
                  --  to the recursion).

                  pragma Assert (Permission (C) = No_Access);
                  null;
               end if;

               pragma Assert (Kind (C) = Entire_Object
                              or else Kind (C) = Record_Component);

               if Kind (C) = Record_Component then
                  --  The tree is unfolded. We just modify the permission and
                  --  return the record subtree.

                  declare
                     Selected_Component : constant Entity_Id :=
                       Entity (Selector_Name (N));

                     Selected_C : Perm_Tree_Access :=
                       Perm_Tree_Maps.Get
                         (Component (C), Selected_Component);

                  begin
                     if Selected_C = null then
                        --  If the hash table returns no element, then we fall
                        --  into the part of Other_Components.
                        pragma Assert (Is_Tagged_Type (Etype (Prefix (N))));

                        Selected_C := Other_Components (C);
                     end if;

                     pragma Assert (Selected_C /= null);

                     --  The Selected_C can have permissions:
                     --  RW : first move in this path
                     --  W  : Already other moves in this path
                     --  No : Already other moves with 'Access

                     pragma Assert (Permission (Selected_C) /= Read_Only);
                     if Mode = Super_Move then
                        Selected_C.all.Tree.Permission := No_Access;
                     else
                        Selected_C.all.Tree.Permission :=
                          Glb (Write_Only, Permission (Selected_C));

                     end if;

                     return Selected_C;
                  end;
               elsif Kind (C) = Entire_Object then
                  declare
                     --  Expand the tree. Replace the node with
                     --  Record_Component.

                     Elem : Node_Id;

                     --  Create an empty hash table

                     Hashtbl : Perm_Tree_Maps.Instance;

                     --  We are in Move or Super_Move mode, hence we can assume
                     --  that the Children_permission is RW, given that there
                     --  are no other paths that could have been moved.

                     pragma Assert (Children_Permission (C) = Read_Write);

                     --  We create the unrolled nodes, that will all have RW
                     --  permission given that we are in move mode. We will
                     --  then set the right node to W.

                     Son : Perm_Tree_Access;

                  begin
                     --  We change the current node from Entire_Object to
                     --  Record_Component with same permission and an empty
                     --  hash table as component list.

                     C.all.Tree :=
                       (Kind             => Record_Component,
                        Is_Node_Deep     => Is_Node_Deep (C),
                        Permission       => Permission (C),
                        Component        => Hashtbl,
                        Other_Components =>
                           new Perm_Tree_Wrapper'
                          (Tree =>
                               (Kind                => Entire_Object,
                                Is_Node_Deep        => True,
                                Permission          => Read_Write,
                                Children_Permission => Read_Write)
                          ));

                     --  We fill the hash table with all sons of the record,
                     --  with basic Entire_Objects nodes.
                     Elem := First_Component_Or_Discriminant
                       (Etype (Prefix (N)));

                     while Present (Elem) loop
                        Son := new Perm_Tree_Wrapper'
                          (Tree =>
                             (Kind                => Entire_Object,
                              Is_Node_Deep        => Is_Deep (Etype (Elem)),
                              Permission          => Read_Write,
                              Children_Permission => Read_Write));

                        Perm_Tree_Maps.Set
                          (C.all.Tree.Component, Elem, Son);

                        Next_Component_Or_Discriminant (Elem);
                     end loop;

                     --  Now we set the right field to Write_Only or No_Access
                     --  depending on mode, and then we return the tree to the
                     --  sons, so that the recursion can continue.

                     declare
                        Selected_Component : constant Entity_Id :=
                          Entity (Selector_Name (N));

                        Selected_C : Perm_Tree_Access :=
                          Perm_Tree_Maps.Get
                            (Component (C), Selected_Component);

                     begin
                        if Selected_C = null then
                           Selected_C := Other_Components (C);
                        end if;

                        pragma Assert (Selected_C /= null);

                        --  Given that this is a newly created Select_C, we can
                        --  safely assume that its permission is Read_Write.

                        pragma Assert (Permission (Selected_C) =
                                         Read_Write);

                        if Mode = Super_Move then
                           Selected_C.all.Tree.Permission := No_Access;
                        else
                           Selected_C.all.Tree.Permission := Write_Only;
                        end if;

                        return Selected_C;
                     end;
                  end;
               else
                  raise Program_Error;
               end if;
            end;

            --  We set the permission tree of its prefix, and then we extract
            --  from the returned pointer the subtree and assign an adequate
            --  permission to it, if unfolded. If folded, we unroll the tree
            --  at one step.

         when N_Indexed_Component
            | N_Slice
         =>
            declare
               C : constant Perm_Tree_Access :=
                 Set_Perm_Prefixes_Move (Prefix (N), Mode);

            begin
               if C = null then
                  --  We went through a function call, do nothing

                  return null;
               end if;

               --  The permission of the returned node should be either
               --  W (because the recursive call sets <= Write_Only)
               --  or No (if another path has been moved with 'Access)

               if Mode = Super_Move then
                  pragma Assert (Permission (C) = No_Access);
                  null;
               else
                  pragma Assert (Permission (C) = Write_Only
                                 or else Permission (C) = No_Access);
                  null;
               end if;

               pragma Assert (Kind (C) = Entire_Object
                              or else Kind (C) = Array_Component);

               if Kind (C) = Array_Component then
                  --  The tree is unfolded. We just modify the permission and
                  --  return the elem subtree.

                  if Get_Elem (C) = null then
                     --  Hash_Table_Error
                     raise Program_Error;
                  end if;

                  --  The Get_Elem can have permissions :
                  --  RW : first move in this path
                  --  W  : Already other moves in this path
                  --  No : Already other moves with 'Access

                  pragma Assert (Permission (Get_Elem (C)) /= Read_Only);

                  if Mode = Super_Move then
                     C.all.Tree.Get_Elem.all.Tree.Permission := No_Access;
                  else
                     C.all.Tree.Get_Elem.all.Tree.Permission :=
                       Glb (Write_Only, Permission (Get_Elem (C)));
                  end if;

                  return Get_Elem (C);
               elsif Kind (C) = Entire_Object then
                  declare
                     --  Expand the tree. Replace node with Array_Component.

                     --  We are in move mode, hence we can assume that the
                     --  Children_permission is RW.

                     pragma Assert (Children_Permission (C) = Read_Write);

                     Son : Perm_Tree_Access;

                  begin
                     Son := new Perm_Tree_Wrapper'
                       (Tree =>
                          (Kind                => Entire_Object,
                           Is_Node_Deep        => Is_Node_Deep (C),
                           Permission          => Read_Write,
                           Children_Permission => Read_Write));

                     if Mode = Super_Move then
                        Son.all.Tree.Permission := No_Access;
                     else
                        Son.all.Tree.Permission := Write_Only;
                     end if;

                     --  We change the current node from Entire_Object
                     --  to Array_Component with same permission and the
                     --  previously defined son.

                     C.all.Tree := (Kind         => Array_Component,
                                    Is_Node_Deep => Is_Node_Deep (C),
                                    Permission   => Permission (C),
                                    Get_Elem     => Son);

                     return Get_Elem (C);
                  end;
               else
                  raise Program_Error;
               end if;
            end;

            --  We set the permission tree of its prefix, and then we extract
            --  from the returned pointer the subtree and assign an adequate
            --  permission to it, if unfolded. If folded, we unroll the tree
            --  at one step.

         when N_Explicit_Dereference =>
            declare
               C : constant Perm_Tree_Access :=
                 Set_Perm_Prefixes_Move (Prefix (N), Move);

            begin
               if C = null then
                  --  We went through a function call: do nothing

                  return null;
               end if;

               --  The permission of the returned node should be only
               --  W (because the recursive call sets <= Write_Only)
               --  No is NOT POSSIBLE here

               pragma Assert (Permission (C) = Write_Only);

               pragma Assert (Kind (C) = Entire_Object
                              or else Kind (C) = Reference);

               if Kind (C) = Reference then
                  --  The tree is unfolded. We just modify the permission and
                  --  return the elem subtree.

                  if Get_All (C) = null then
                     --  Hash_Table_Error
                     raise Program_Error;
                  end if;

                  --  The Get_All can have permissions :
                  --  RW : first move in this path
                  --  W  : Already other moves in this path
                  --  No : Already other moves with 'Access

                  pragma Assert (Permission (Get_All (C)) /= Read_Only);

                  if Mode = Super_Move then
                     C.all.Tree.Get_All.all.Tree.Permission := No_Access;
                  else
                     Get_All (C).all.Tree.Permission :=
                       Glb (Write_Only, Permission (Get_All (C)));
                  end if;
                  return Get_All (C);
               elsif Kind (C) = Entire_Object then
                  declare
                     --  Expand the tree. Replace the node with Reference.

                     --  We are in Move or Super_Move mode, hence we can assume
                     --  that the Children_permission is RW.

                     pragma Assert (Children_Permission (C) = Read_Write);

                     Son : Perm_Tree_Access;

                  begin
                     Son := new Perm_Tree_Wrapper'
                       (Tree =>
                          (Kind                => Entire_Object,
                           Is_Node_Deep        => Is_Deep (Etype (N)),
                           Permission          => Read_Write,
                           Children_Permission => Read_Write));

                     if Mode = Super_Move then
                        Son.all.Tree.Permission := No_Access;
                     else
                        Son.all.Tree.Permission := Write_Only;
                     end if;

                     --  We change the current node from Entire_Object to
                     --  Reference with Write_Only and the previous son.

                     pragma Assert (Is_Node_Deep (C));

                     C.all.Tree := (Kind         => Reference,
                                    Is_Node_Deep => Is_Node_Deep (C),
                                    Permission   => Write_Only,
                                    --  Write_only is equal to C.Permission
                                    Get_All      => Son);

                     return Get_All (C);
                  end;
               else
                  raise Program_Error;
               end if;
            end;

         when N_Function_Call =>
            return null;

         when others =>
            raise Program_Error;
      end case;

   end Set_Perm_Prefixes_Move;

   -------------------------------
   -- Set_Perm_Prefixes_Observe --
   -------------------------------

   function Set_Perm_Prefixes_Observe
     (N : Node_Id)
      return Perm_Tree_Access
   is
   begin
      pragma Assert (Current_Checking_Mode = Observe);

      case Nkind (N) is
         --  Base identifier. Set permission to R.

         when N_Identifier
            | N_Expanded_Name
            | N_Defining_Identifier
         =>
            declare
               P : Node_Id;
               C : Perm_Tree_Access;

            begin
               if Nkind (N) = N_Defining_Identifier then
                  P := N;
               else
                  P := Entity (N);
               end if;

               C := Get (Current_Perm_Env, Unique_Entity (P));
               --  Setting the initialization map to True, so that this
               --  variable cannot be ignored anymore when looking at end
               --  of elaboration of package.

               Set (Current_Initialization_Map, Unique_Entity (P), True);

               if C = null then
                  --  No null possible here, there are no parents for the path.
                  --  This means we are using a global variable without adding
                  --  it in environment with a global aspect.

                  Illegal_Global_Usage (N);
               end if;

               C.all.Tree.Permission := Glb (Read_Only, Permission (C));

               return C;
            end;

         when N_Type_Conversion
            | N_Unchecked_Type_Conversion
            | N_Qualified_Expression
         =>
            return Set_Perm_Prefixes_Observe (Expression (N));

         when N_Parameter_Specification =>
            raise Program_Error;

            --  We set the permission tree of its prefix, and then we extract
            --  from the returned pointer our subtree and assign an adequate
            --  permission to it, if unfolded. If folded, we unroll the tree
            --  at one step.

         when N_Selected_Component =>
            declare
               C : constant Perm_Tree_Access :=
                 Set_Perm_Prefixes_Observe (Prefix (N));

            begin
               if C = null then
                  --  We went through a function call, do nothing

                  return null;
               end if;

               pragma Assert (Kind (C) = Entire_Object
                              or else Kind (C) = Record_Component);

               if Kind (C) = Record_Component then
                  --  The tree is unfolded. We just modify the permission and
                  --  return the record subtree. We put the permission to the
                  --  glb of read_only and its current permission, to consider
                  --  the case of observing x.y while x.z has been moved. Then
                  --  x should be No_Access.

                  declare
                     Selected_Component : constant Entity_Id :=
                       Entity (Selector_Name (N));

                     Selected_C : Perm_Tree_Access :=
                       Perm_Tree_Maps.Get
                         (Component (C), Selected_Component);

                  begin
                     if Selected_C = null then
                        Selected_C := Other_Components (C);
                     end if;

                     pragma Assert (Selected_C /= null);

                     Selected_C.all.Tree.Permission :=
                       Glb (Read_Only, Permission (Selected_C));

                     return Selected_C;
                  end;
               elsif Kind (C) = Entire_Object then
                  declare
                     --  Expand the tree. Replace the node with
                     --  Record_Component.

                     Elem : Node_Id;

                     --  Create an empty hash table

                     Hashtbl : Perm_Tree_Maps.Instance;

                     --  We create the unrolled nodes, that will all have RW
                     --  permission given that we are in move mode. We will
                     --  then set the right node to W.

                     Son : Perm_Tree_Access;

                     Child_Perm : constant Perm_Kind :=
                       Children_Permission (C);

                  begin
                     --  We change the current node from Entire_Object to
                     --  Record_Component with same permission and an empty
                     --  hash table as component list.

                     C.all.Tree :=
                       (Kind             => Record_Component,
                        Is_Node_Deep     => Is_Node_Deep (C),
                        Permission       => Permission (C),
                        Component        => Hashtbl,
                        Other_Components =>
                           new Perm_Tree_Wrapper'
                          (Tree =>
                               (Kind                => Entire_Object,
                                Is_Node_Deep        => True,
                                Permission          => Child_Perm,
                                Children_Permission => Child_Perm)
                          ));

                     --  We fill the hash table with all sons of the record,
                     --  with basic Entire_Objects nodes.
                     Elem := First_Component_Or_Discriminant
                       (Etype (Prefix (N)));

                     while Present (Elem) loop
                        Son := new Perm_Tree_Wrapper'
                          (Tree =>
                             (Kind                => Entire_Object,
                              Is_Node_Deep        => Is_Deep (Etype (Elem)),
                              Permission          => Child_Perm,
                              Children_Permission => Child_Perm));

                        Perm_Tree_Maps.Set
                          (C.all.Tree.Component, Elem, Son);

                        Next_Component_Or_Discriminant (Elem);
                     end loop;

                     --  Now we set the right field to Read_Only. and then we
                     --  return the tree to the sons, so that the recursion can
                     --  continue.

                     declare
                        Selected_Component : constant Entity_Id :=
                          Entity (Selector_Name (N));

                        Selected_C : Perm_Tree_Access :=
                          Perm_Tree_Maps.Get
                            (Component (C), Selected_Component);

                     begin
                        if Selected_C = null then
                           Selected_C := Other_Components (C);
                        end if;

                        pragma Assert (Selected_C /= null);

                        Selected_C.all.Tree.Permission :=
                          Glb (Read_Only, Child_Perm);

                        return Selected_C;
                     end;
                  end;
               else
                  raise Program_Error;
               end if;
            end;

         --  We set the permission tree of its prefix, and then we extract from
         --  the returned pointer the subtree and assign an adequate permission
         --  to it, if unfolded. If folded, we unroll the tree at one step.

         when N_Indexed_Component
            | N_Slice
         =>
            declare
               C : constant Perm_Tree_Access :=
                 Set_Perm_Prefixes_Observe (Prefix (N));

            begin
               if C = null then
                  --  We went through a function call, do nothing

                  return null;
               end if;

               pragma Assert (Kind (C) = Entire_Object
                              or else Kind (C) = Array_Component);

               if Kind (C) = Array_Component then
                  --  The tree is unfolded. We just modify the permission and
                  --  return the elem subtree.

                  pragma Assert (Get_Elem (C) /= null);

                  C.all.Tree.Get_Elem.all.Tree.Permission :=
                    Glb (Read_Only, Permission (Get_Elem (C)));

                  return Get_Elem (C);
               elsif Kind (C) = Entire_Object then
                  declare
                     --  Expand the tree. Replace node with Array_Component.

                     Son : Perm_Tree_Access;

                     Child_Perm : constant Perm_Kind :=
                       Glb (Read_Only, Children_Permission (C));

                  begin
                     Son := new Perm_Tree_Wrapper'
                       (Tree =>
                          (Kind                => Entire_Object,
                           Is_Node_Deep        => Is_Node_Deep (C),
                           Permission          => Child_Perm,
                           Children_Permission => Child_Perm));

                     --  We change the current node from Entire_Object
                     --  to Array_Component with same permission and the
                     --  previously defined son.

                     C.all.Tree := (Kind         => Array_Component,
                                    Is_Node_Deep => Is_Node_Deep (C),
                                    Permission   => Child_Perm,
                                    Get_Elem     => Son);

                     return Get_Elem (C);
                  end;

               else
                  raise Program_Error;
               end if;
            end;

         --  We set the permission tree of its prefix, and then we extract from
         --  the returned pointer the subtree and assign an adequate permission
         --  to it, if unfolded. If folded, we unroll the tree at one step.

         when N_Explicit_Dereference =>
            declare
               C : constant Perm_Tree_Access :=
                 Set_Perm_Prefixes_Observe (Prefix (N));

            begin
               if C = null then
                  --  We went through a function call, do nothing

                  return null;
               end if;

               pragma Assert (Kind (C) = Entire_Object
                              or else Kind (C) = Reference);

               if Kind (C) = Reference then
                  --  The tree is unfolded. We just modify the permission and
                  --  return the elem subtree.

                  pragma Assert (Get_All (C) /= null);

                  C.all.Tree.Get_All.all.Tree.Permission :=
                    Glb (Read_Only, Permission (Get_All (C)));

                  return Get_All (C);
               elsif Kind (C) = Entire_Object then
                  declare
                     --  Expand the tree. Replace the node with Reference.

                     Son : Perm_Tree_Access;

                     Child_Perm : constant Perm_Kind :=
                       Glb (Read_Only, Children_Permission (C));

                  begin
                     Son := new Perm_Tree_Wrapper'
                       (Tree =>
                          (Kind                => Entire_Object,
                           Is_Node_Deep        => Is_Deep (Etype (N)),
                           Permission          => Child_Perm,
                           Children_Permission => Child_Perm));

                     --  We change the current node from Entire_Object to
                     --  Reference with Write_Only and the previous son.

                     pragma Assert (Is_Node_Deep (C));

                     C.all.Tree := (Kind         => Reference,
                                    Is_Node_Deep => Is_Node_Deep (C),
                                    Permission   => Child_Perm,
                                    Get_All      => Son);

                     return Get_All (C);
                  end;
               else
                  raise Program_Error;
               end if;
            end;

         when N_Function_Call =>
            return null;

         when others =>
            raise Program_Error;

      end case;
   end Set_Perm_Prefixes_Observe;

   -------------------
   -- Setup_Globals --
   -------------------

   procedure Setup_Globals (Subp : Entity_Id) is

      procedure Setup_Globals_From_List
        (First_Item : Node_Id;
         Kind       : Formal_Kind);
      --  Set up global items from the list starting at Item

      procedure Setup_Globals_Of_Mode (Global_Mode : Name_Id);
      --  Set up global items for the mode Global_Mode

      -----------------------------
      -- Setup_Globals_From_List --
      -----------------------------

      procedure Setup_Globals_From_List
        (First_Item : Node_Id;
         Kind       : Formal_Kind)
      is
         Item : Node_Id := First_Item;
         E    : Entity_Id;

      begin
         while Present (Item) loop
            E := Entity (Item);

            --  Ignore abstract states, which play no role in pointer aliasing

            if Ekind (E) = E_Abstract_State then
               null;
            else
               Setup_Parameter_Or_Global (E, Kind);
            end if;
            Next_Global (Item);
         end loop;
      end Setup_Globals_From_List;

      ---------------------------
      -- Setup_Globals_Of_Mode --
      ---------------------------

      procedure Setup_Globals_Of_Mode (Global_Mode : Name_Id) is
         Kind : Formal_Kind;

      begin
         case Global_Mode is
            when Name_Input | Name_Proof_In =>
               Kind := E_In_Parameter;
            when Name_Output =>
               Kind := E_Out_Parameter;
            when Name_In_Out =>
               Kind := E_In_Out_Parameter;
            when others =>
               raise Program_Error;
         end case;

         --  Set up both global items from Global and Refined_Global pragmas

         Setup_Globals_From_List (First_Global (Subp, Global_Mode), Kind);
         Setup_Globals_From_List
           (First_Global (Subp, Global_Mode, Refined => True), Kind);
      end Setup_Globals_Of_Mode;

   --  Start of processing for Setup_Globals

   begin
      Setup_Globals_Of_Mode (Name_Proof_In);
      Setup_Globals_Of_Mode (Name_Input);
      Setup_Globals_Of_Mode (Name_Output);
      Setup_Globals_Of_Mode (Name_In_Out);
   end Setup_Globals;

   -------------------------------
   -- Setup_Parameter_Or_Global --
   -------------------------------

   procedure Setup_Parameter_Or_Global
     (Id   : Entity_Id;
      Mode : Formal_Kind)
   is
      Elem : Perm_Tree_Access;

   begin
      Elem := new Perm_Tree_Wrapper'
        (Tree =>
           (Kind                => Entire_Object,
            Is_Node_Deep        => Is_Deep (Etype (Id)),
            Permission          => Read_Write,
            Children_Permission => Read_Write));

      case Mode is
         when E_In_Parameter =>

            --  Borrowed IN: RW for everybody

            if Is_Borrowed_In (Id) then
               Elem.all.Tree.Permission := Read_Write;
               Elem.all.Tree.Children_Permission := Read_Write;

            --  Observed IN: R for everybody

            else
               Elem.all.Tree.Permission := Read_Only;
               Elem.all.Tree.Children_Permission := Read_Only;
            end if;

         --  OUT: borrow, but callee has W only

         when E_Out_Parameter =>
            Elem.all.Tree.Permission := Write_Only;
            Elem.all.Tree.Children_Permission := Write_Only;

         --  IN OUT: borrow and callee has RW

         when E_In_Out_Parameter =>
            Elem.all.Tree.Permission := Read_Write;
            Elem.all.Tree.Children_Permission := Read_Write;
      end case;

      Set (Current_Perm_Env, Id, Elem);
   end Setup_Parameter_Or_Global;

   ----------------------
   -- Setup_Parameters --
   ----------------------

   procedure Setup_Parameters (Subp : Entity_Id) is
      Formal : Entity_Id;

   begin
      Formal := First_Formal (Subp);
      while Present (Formal) loop
         Setup_Parameter_Or_Global (Formal, Ekind (Formal));
         Next_Formal (Formal);
      end loop;
   end Setup_Parameters;

end Sem_SPARK;
