------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ S C I L                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2009-2024, Free Software Foundation, Inc.         --
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

with Einfo;          use Einfo;
with Einfo.Entities; use Einfo.Entities;
with Einfo.Utils;    use Einfo.Utils;
with Nlists;         use Nlists;
with Rtsfind;        use Rtsfind;
with Sem_Aux;        use Sem_Aux;
with Sinfo;          use Sinfo;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Stand;          use Stand;
with SCIL_LL;        use SCIL_LL;

package body Sem_SCIL is

   ---------------------
   -- Check_SCIL_Node --
   ---------------------

   function Check_SCIL_Node (N : Node_Id) return Traverse_Result is
      SCIL_Node : constant Node_Id := Get_SCIL_Node (N);
      Ctrl_Tag  : Node_Id;
      Ctrl_Typ  : Entity_Id;

   begin
      --  For nodes that do not have SCIL node continue traversing the tree

      if No (SCIL_Node) then
         return OK;
      end if;

      case Nkind (SCIL_Node) is
         when N_SCIL_Dispatch_Table_Tag_Init =>
            pragma Assert (Nkind (N) = N_Object_Declaration);
            null;

         when N_SCIL_Dispatching_Call =>
            Ctrl_Tag := SCIL_Controlling_Tag (SCIL_Node);

            --  Parent of SCIL dispatching call nodes MUST be a subprogram call

            if Nkind (N) not in N_Subprogram_Call then
               raise Program_Error;

            --  In simple cases the controlling tag is the tag of the
            --  controlling argument (i.e. Obj.Tag).

            elsif Nkind (Ctrl_Tag) = N_Selected_Component then
               Ctrl_Typ := Etype (Ctrl_Tag);

               --  Interface types are unsupported

               if Is_Interface (Ctrl_Typ)
                 or else Is_RTE (Ctrl_Typ, RE_Interface_Tag)
               then
                  null;

               else
                  pragma Assert (Is_RTE (Ctrl_Typ, RE_Tag));
                  null;
               end if;

            --  When the controlling tag of a dispatching call is an identifier
            --  the SCIL_Controlling_Tag attribute references the corresponding
            --  object or parameter declaration. Interface types are still
            --  unsupported.

            elsif Nkind (Ctrl_Tag) in N_Object_Renaming_Declaration
                                    | N_Object_Declaration
                                    | N_Parameter_Specification
                                    | N_Discriminant_Specification
            then
               Ctrl_Typ := Etype (Defining_Identifier (Ctrl_Tag));

               --  Interface types are unsupported.

               if Is_Interface (Ctrl_Typ)
                 or else From_Limited_With (Ctrl_Typ)
                 or else Is_RTE (Ctrl_Typ, RE_Interface_Tag)
                 or else (Is_Access_Type (Ctrl_Typ)
                           and then
                             Is_Interface
                               (Available_View
                                 (Base_Type (Designated_Type (Ctrl_Typ)))))
               then
                  null;

               else
                  pragma Assert
                    (Is_RTE (Ctrl_Typ, RE_Tag)
                       or else
                         (Is_Access_Type (Ctrl_Typ)
                            and then
                          Is_RTE
                            (Available_View
                               (Base_Type (Designated_Type (Ctrl_Typ))),
                             RE_Tag)));
                  null;
               end if;

            --  Interface types are unsupported

            elsif Is_Interface (Etype (Ctrl_Tag)) then
               null;

            else
               pragma Assert (False);
               raise Program_Error;
            end if;

            return Skip;

         when N_SCIL_Membership_Test =>

            --  Check contents of the boolean expression associated with the
            --  membership test.

            pragma Assert
              (Nkind (N) in
                 N_Identifier | N_And_Then | N_Or_Else |
                 N_Expression_With_Actions | N_Function_Call
              and then Etype (N) = Standard_Boolean);

            --  Check the entity identifier of the associated tagged type (that
            --  is, in testing for membership in T'Class, the entity id of the
            --  specific type T).

            --  Note: When the SCIL node is generated the private and full-view
            --    of the tagged types may have been swapped and hence the node
            --    referenced by attribute SCIL_Entity may be the private view.
            --    Therefore, in order to uniformly locate the full-view we use
            --    attribute Underlying_Type.

            pragma Assert
              (Is_Tagged_Type (Underlying_Type (SCIL_Entity (SCIL_Node))));

            --  Interface types are unsupported

            pragma Assert
              (not Is_Interface (Underlying_Type (SCIL_Entity (SCIL_Node))));

            --  Check the decoration of the expression that denotes the tag
            --  value being tested

            Ctrl_Tag := SCIL_Tag_Value (SCIL_Node);

            case Nkind (Ctrl_Tag) is

               --  For class-wide membership tests the SCIL tag value is the
               --  tag of the tested object (i.e. Obj.Tag).

               when N_Selected_Component =>
                  pragma Assert (Is_RTE (Etype (Ctrl_Tag), RE_Tag));
                  null;

               when others =>
                  pragma Assert (False);
                  null;
            end case;

            return Skip;

         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;

      return Skip;
   end Check_SCIL_Node;

   -------------------------
   -- First_Non_SCIL_Node --
   -------------------------

   function First_Non_SCIL_Node (L : List_Id) return Node_Id is
      N : Node_Id;

   begin
      N := First (L);
      while Nkind (N) in N_SCIL_Node loop
         Next (N);
      end loop;

      return N;
   end First_Non_SCIL_Node;

   ------------------------
   -- Next_Non_SCIL_Node --
   ------------------------

   function Next_Non_SCIL_Node (N : Node_Id) return Node_Id is
      Aux_N : Node_Id;

   begin
      Aux_N := Next (N);
      while Nkind (Aux_N) in N_SCIL_Node loop
         Next (Aux_N);
      end loop;

      return Aux_N;
   end Next_Non_SCIL_Node;

end Sem_SCIL;
