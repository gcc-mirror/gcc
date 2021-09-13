------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                 L I V E                                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2000-2021, Free Software Foundation, Inc.         --
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

with Atree;          use Atree;
with Einfo;          use Einfo;
with Einfo.Entities; use Einfo.Entities;
with Einfo.Utils;    use Einfo.Utils;
with Lib;            use Lib;
with Nlists;         use Nlists;
with Sem_Aux;        use Sem_Aux;
with Sem_Util;       use Sem_Util;
with Sinfo;          use Sinfo;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinfo.Utils;    use Sinfo.Utils;
with Types;          use Types;

package body Live is

   --  Name_Set

   --  The Name_Set type is used to store the temporary mark bits used by the
   --  garbage collection of entities. Using a separate array prevents using up
   --  any valuable per-node space and possibly results in better locality and
   --  cache usage.

   type Name_Set is array (Node_Id range <>) of Boolean;
   pragma Pack (Name_Set);

   function Marked (Marks : Name_Set; Name : Node_Id) return Boolean;
   pragma Inline (Marked);

   procedure Set_Marked
     (Marks : in out Name_Set;
      Name  : Node_Id;
      Mark  : Boolean := True);
   pragma Inline (Set_Marked);

   --  Algorithm

   --  The problem of finding live entities is solved in two steps:

   procedure Mark (Root : Node_Id; Marks : out Name_Set);
   --  Mark all live entities in Root as Marked

   procedure Sweep (Root : Node_Id; Marks : Name_Set);
   --  For all unmarked entities in Root set Is_Eliminated to true

   --  The Mark phase is split into two phases:

   procedure Init_Marked (Root : Node_Id; Marks : out Name_Set);
   --  For all subprograms, reset Is_Public flag if a pragma Eliminate applies
   --  to the entity, and set the Marked flag to Is_Public.

   procedure Trace_Marked (Root : Node_Id; Marks : in out Name_Set);
   --  Traverse the tree skipping any unmarked subprogram bodies. All visited
   --  entities are marked, as well as entities denoted by a visited identifier
   --  or operator. When an entity is first marked it is traced as well.

   --  Local functions

   function Body_Of (E : Entity_Id) return Node_Id;
   --  Returns subprogram body corresponding to entity E

   function Spec_Of (N : Node_Id) return Entity_Id;
   --  Given a subprogram body N, return defining identifier of its declaration

   -------------
   -- Body_Of --
   -------------

   function Body_Of (E : Entity_Id) return Node_Id is
      Decl   : constant Node_Id   := Unit_Declaration_Node (E);
      Kind   : constant Node_Kind := Nkind (Decl);
      Result : Node_Id;

   begin
      if Kind = N_Subprogram_Body then
         Result := Decl;

      elsif Kind /= N_Subprogram_Declaration
        and  Kind /= N_Subprogram_Body_Stub
      then
         Result := Empty;

      else
         Result := Corresponding_Body (Decl);

         if Result /= Empty then
            Result := Unit_Declaration_Node (Result);
         end if;
      end if;

      return Result;
   end Body_Of;

   ------------------------------
   -- Collect_Garbage_Entities --
   ------------------------------

   procedure Collect_Garbage_Entities is
      Root  : constant Node_Id := Cunit (Main_Unit);
      Marks : Name_Set (0 .. Last_Node_Id);

   begin
      Mark (Root, Marks);
      Sweep (Root, Marks);
   end Collect_Garbage_Entities;

   -----------------
   -- Init_Marked --
   -----------------

   procedure Init_Marked (Root : Node_Id; Marks : out Name_Set) is

      function Process (N : Node_Id) return Traverse_Result;
      procedure Traverse is new Traverse_Proc (Process);

      -------------
      -- Process --
      -------------

      function Process (N : Node_Id) return Traverse_Result is
      begin
         case Nkind (N) is
            when N_Entity'Range =>
               if Is_Eliminated (N) then
                  Set_Is_Public (N, False);
               end if;

               Set_Marked (Marks, N, Is_Public (N));

            when N_Subprogram_Body =>
               Traverse (Spec_Of (N));

            when N_Package_Body_Stub =>
               if Present (Library_Unit (N)) then
                  Traverse (Proper_Body (Unit (Library_Unit (N))));
               end if;

            when N_Package_Body =>
               declare
                  Elmt : Node_Id := First (Declarations (N));
               begin
                  while Present (Elmt) loop
                     Traverse (Elmt);
                     Next (Elmt);
                  end loop;
               end;

            when others =>
               null;
         end case;

         return OK;
      end Process;

   --  Start of processing for Init_Marked

   begin
      Marks := (others => False);
      Traverse (Root);
   end Init_Marked;

   ----------
   -- Mark --
   ----------

   procedure Mark (Root : Node_Id; Marks : out Name_Set) is
   begin
      Init_Marked (Root, Marks);
      Trace_Marked (Root, Marks);
   end Mark;

   ------------
   -- Marked --
   ------------

   function Marked (Marks : Name_Set; Name : Node_Id) return Boolean is
   begin
      return Marks (Name);
   end Marked;

   ----------------
   -- Set_Marked --
   ----------------

   procedure Set_Marked
     (Marks : in out Name_Set;
      Name  : Node_Id;
      Mark  : Boolean := True)
   is
   begin
      Marks (Name) := Mark;
   end Set_Marked;

   -------------
   -- Spec_Of --
   -------------

   function Spec_Of (N : Node_Id) return Entity_Id is
   begin
      if Acts_As_Spec (N) then
         return Defining_Entity (N);
      else
         return Corresponding_Spec (N);
      end if;
   end Spec_Of;

   -----------
   -- Sweep --
   -----------

   procedure Sweep (Root : Node_Id; Marks : Name_Set) is

      function Process (N : Node_Id) return Traverse_Result;
      procedure Traverse is new Traverse_Proc (Process);

      -------------
      -- Process --
      -------------

      function Process (N : Node_Id) return Traverse_Result is
      begin
         case Nkind (N) is
            when N_Entity'Range =>
               Set_Is_Eliminated (N, not Marked (Marks, N));

            when N_Subprogram_Body =>
               Traverse (Spec_Of (N));

            when N_Package_Body_Stub =>
               if Present (Library_Unit (N)) then
                  Traverse (Proper_Body (Unit (Library_Unit (N))));
               end if;

            when N_Package_Body =>
               declare
                  Elmt : Node_Id := First (Declarations (N));
               begin
                  while Present (Elmt) loop
                     Traverse (Elmt);
                     Next (Elmt);
                  end loop;
               end;

            when others =>
               null;
         end case;

         return OK;
      end Process;

   --  Start of processing for Sweep

   begin
      Traverse (Root);
   end Sweep;

   ------------------
   -- Trace_Marked --
   ------------------

   procedure Trace_Marked (Root : Node_Id; Marks : in out Name_Set) is

      function  Process (N : Node_Id) return Traverse_Result;
      procedure Process (N : Node_Id);
      procedure Traverse is new Traverse_Proc (Process);

      -------------
      -- Process --
      -------------

      procedure Process (N : Node_Id) is
         Result : Traverse_Result;
         pragma Warnings (Off, Result);

      begin
         Result := Process (N);
      end Process;

      function Process (N : Node_Id) return Traverse_Result is
         Result : Traverse_Result := OK;
         B      : Node_Id;
         E      : Entity_Id;

      begin
         case Nkind (N) is
            when N_Generic_Declaration'Range
               | N_Pragma
               | N_Subprogram_Body_Stub
               | N_Subprogram_Declaration
            =>
               Result := Skip;

            when N_Subprogram_Body =>
               if not Marked (Marks, Spec_Of (N)) then
                  Result := Skip;
               end if;

            when N_Package_Body_Stub =>
               if Present (Library_Unit (N)) then
                  Traverse (Proper_Body (Unit (Library_Unit (N))));
               end if;

            when N_Expanded_Name
               | N_Identifier
               | N_Operator_Symbol
            =>
               E := Entity (N);

               if E /= Empty and then not Marked (Marks, E) then
                  Process (E);

                  if Is_Subprogram (E) then
                     B := Body_Of (E);

                     if B /= Empty then
                        Traverse (B);
                     end if;
                  end if;
               end if;

            when N_Entity'Range =>
               if (Ekind (N) = E_Component) and then not Marked (Marks, N) then
                  if Present (Discriminant_Checking_Func (N)) then
                     Process (Discriminant_Checking_Func (N));
                  end if;
               end if;

               Set_Marked (Marks, N);

            when others =>
               null;
         end case;

         return Result;
      end Process;

   --  Start of processing for Trace_Marked

   begin
      Traverse (Root);
   end Trace_Marked;

end Live;
