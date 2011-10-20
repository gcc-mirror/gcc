------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P A R . C H 1 2                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2011, Free Software Foundation, Inc.         --
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

pragma Style_Checks (All_Checks);
--  Turn off subprogram body ordering check. Subprograms are in order
--  by RM section rather than alphabetical

separate (Par)
package body Ch12 is

   --  Local functions, used only in this chapter

   function P_Formal_Derived_Type_Definition           return Node_Id;
   function P_Formal_Discrete_Type_Definition          return Node_Id;
   function P_Formal_Fixed_Point_Definition            return Node_Id;
   function P_Formal_Floating_Point_Definition         return Node_Id;
   function P_Formal_Modular_Type_Definition           return Node_Id;
   function P_Formal_Package_Declaration               return Node_Id;
   function P_Formal_Private_Type_Definition           return Node_Id;
   function P_Formal_Signed_Integer_Type_Definition    return Node_Id;
   function P_Formal_Subprogram_Declaration            return Node_Id;
   function P_Formal_Type_Declaration                  return Node_Id;
   function P_Formal_Type_Definition                   return Node_Id;
   function P_Generic_Association                      return Node_Id;

   procedure P_Formal_Object_Declarations (Decls : List_Id);
   --  Scans one or more formal object declarations and appends them to
   --  Decls. Scans more than one declaration only in the case where the
   --  source has a declaration with multiple defining identifiers.

   --------------------------------
   -- 12.1  Generic (also 8.5.5) --
   --------------------------------

   --  This routine parses either one of the forms of a generic declaration
   --  or a generic renaming declaration.

   --  GENERIC_DECLARATION ::=
   --    GENERIC_SUBPROGRAM_DECLARATION | GENERIC_PACKAGE_DECLARATION

   --  GENERIC_SUBPROGRAM_DECLARATION ::=
   --    GENERIC_FORMAL_PART SUBPROGRAM_SPECIFICATION
   --      [ASPECT_SPECIFICATIONS];

   --  GENERIC_PACKAGE_DECLARATION ::=
   --    GENERIC_FORMAL_PART PACKAGE_SPECIFICATION
   --      [ASPECT_SPECIFICATIONS];

   --  GENERIC_FORMAL_PART ::=
   --    generic {GENERIC_FORMAL_PARAMETER_DECLARATION | USE_CLAUSE}

   --  GENERIC_RENAMING_DECLARATION ::=
   --    generic package DEFINING_PROGRAM_UNIT_NAME
   --      renames generic_package_NAME
   --  | generic procedure DEFINING_PROGRAM_UNIT_NAME
   --      renames generic_procedure_NAME
   --  | generic function DEFINING_PROGRAM_UNIT_NAME
   --      renames generic_function_NAME

   --  GENERIC_FORMAL_PARAMETER_DECLARATION ::=
   --    FORMAL_OBJECT_DECLARATION
   --  | FORMAL_TYPE_DECLARATION
   --  | FORMAL_SUBPROGRAM_DECLARATION
   --  | FORMAL_PACKAGE_DECLARATION

   --  The caller has checked that the initial token is GENERIC

   --  Error recovery: can raise Error_Resync

   function P_Generic return Node_Id is
      Gen_Sloc   : constant Source_Ptr := Token_Ptr;
      Gen_Decl   : Node_Id;
      Decl_Node  : Node_Id;
      Decls      : List_Id;
      Def_Unit   : Node_Id;
      Ren_Token  : Token_Type;
      Scan_State : Saved_Scan_State;

   begin
      Scan; -- past GENERIC

      if Token = Tok_Private then
         Error_Msg_SC -- CODEFIX
           ("PRIVATE goes before GENERIC, not after");
         Scan; -- past junk PRIVATE token
      end if;

      Save_Scan_State (Scan_State); -- at token past GENERIC

      --  Check for generic renaming declaration case

      if Token = Tok_Package
        or else Token = Tok_Function
        or else Token = Tok_Procedure
      then
         Ren_Token := Token;
         Scan; -- scan past PACKAGE, FUNCTION or PROCEDURE

         if Token = Tok_Identifier then
            Def_Unit := P_Defining_Program_Unit_Name;

            Check_Misspelling_Of (Tok_Renames);

            if Token = Tok_Renames then
               if Ren_Token = Tok_Package then
                  Decl_Node := New_Node
                    (N_Generic_Package_Renaming_Declaration, Gen_Sloc);

               elsif Ren_Token = Tok_Procedure then
                  Decl_Node := New_Node
                    (N_Generic_Procedure_Renaming_Declaration, Gen_Sloc);

               else -- Ren_Token = Tok_Function then
                  Decl_Node := New_Node
                    (N_Generic_Function_Renaming_Declaration, Gen_Sloc);
               end if;

               Scan; -- past RENAMES
               Set_Defining_Unit_Name (Decl_Node, Def_Unit);
               Set_Name (Decl_Node, P_Name);
               TF_Semicolon;
               return Decl_Node;
            end if;
         end if;
      end if;

      --  Fall through if this is *not* a generic renaming declaration

      Restore_Scan_State (Scan_State);
      Decls := New_List;

      --  Loop through generic parameter declarations and use clauses

      Decl_Loop : loop
         P_Pragmas_Opt (Decls);

         if Token = Tok_Private then
            Error_Msg_S ("generic private child packages not permitted");
            Scan; -- past PRIVATE
         end if;

         if Token = Tok_Use then
            Append (P_Use_Clause, Decls);
         else
            --  Parse a generic parameter declaration

            if Token = Tok_Identifier then
               P_Formal_Object_Declarations (Decls);

            elsif Token = Tok_Type then
               Append (P_Formal_Type_Declaration, Decls);

            elsif Token = Tok_With then
               Scan; -- past WITH

               if Token = Tok_Package then
                  Append (P_Formal_Package_Declaration, Decls);

               elsif Token = Tok_Procedure or Token = Tok_Function then
                  Append (P_Formal_Subprogram_Declaration, Decls);

               else
                  Error_Msg_BC -- CODEFIX
                    ("FUNCTION, PROCEDURE or PACKAGE expected here");
                  Resync_Past_Semicolon;
               end if;

            elsif Token = Tok_Subtype then
               Error_Msg_SC ("subtype declaration not allowed " &
                                "as generic parameter declaration!");
               Resync_Past_Semicolon;

            else
               exit Decl_Loop;
            end if;
         end if;
      end loop Decl_Loop;

      --  Generic formal part is scanned, scan out subprogram or package spec

      if Token = Tok_Package then
         Gen_Decl := New_Node (N_Generic_Package_Declaration, Gen_Sloc);
         Set_Specification (Gen_Decl, P_Package (Pf_Spcn));

      else
         Gen_Decl := New_Node (N_Generic_Subprogram_Declaration, Gen_Sloc);

         Set_Specification (Gen_Decl, P_Subprogram_Specification);

         if Nkind (Defining_Unit_Name (Specification (Gen_Decl))) =
                                             N_Defining_Program_Unit_Name
           and then Scope.Last > 0
         then
            Error_Msg_SP ("child unit allowed only at library level");
         end if;

         P_Aspect_Specifications (Gen_Decl);
      end if;

      Set_Generic_Formal_Declarations (Gen_Decl, Decls);
      return Gen_Decl;
   end P_Generic;

   -------------------------------
   -- 12.1  Generic Declaration --
   -------------------------------

   --  Parsed by P_Generic (12.1)

   ------------------------------------------
   -- 12.1  Generic Subprogram Declaration --
   ------------------------------------------

   --  Parsed by P_Generic (12.1)

   ---------------------------------------
   -- 12.1  Generic Package Declaration --
   ---------------------------------------

   --  Parsed by P_Generic (12.1)

   -------------------------------
   -- 12.1  Generic Formal Part --
   -------------------------------

   --  Parsed by P_Generic (12.1)

   -------------------------------------------------
   -- 12.1   Generic Formal Parameter Declaration --
   -------------------------------------------------

   --  Parsed by P_Generic (12.1)

   ---------------------------------
   -- 12.3  Generic Instantiation --
   ---------------------------------

   --  Generic package instantiation parsed by P_Package (7.1)
   --  Generic procedure instantiation parsed by P_Subprogram (6.1)
   --  Generic function instantiation parsed by P_Subprogram (6.1)

   -------------------------------
   -- 12.3  Generic Actual Part --
   -------------------------------

   --  GENERIC_ACTUAL_PART ::=
   --    (GENERIC_ASSOCIATION {, GENERIC_ASSOCIATION})

   --  Returns a list of generic associations, or Empty if none are present

   --  Error recovery: cannot raise Error_Resync

   function P_Generic_Actual_Part_Opt return List_Id is
      Association_List : List_Id;

   begin
      --  Figure out if a generic actual part operation is present. Clearly
      --  there is no generic actual part if the current token is semicolon
      --  or if we have aspect specifications present.

      if Token = Tok_Semicolon or else Aspect_Specifications_Present then
         return No_List;

      --  If we don't have a left paren, then we have an error, and the job
      --  is to figure out whether a left paren or semicolon was intended.
      --  We assume a missing left paren (and hence a generic actual part
      --  present) if the current token is not on a new line, or if it is
      --  indented from the subprogram token. Otherwise assume missing
      --  semicolon (which will be diagnosed by caller) and no generic part

      elsif Token /= Tok_Left_Paren
        and then Token_Is_At_Start_Of_Line
        and then Start_Column <= Scope.Table (Scope.Last).Ecol
      then
         return No_List;

      --  Otherwise we have a generic actual part (either a left paren is
      --  present, or we have decided that there must be a missing left paren)

      else
         Association_List := New_List;
         T_Left_Paren;

         loop
            Append (P_Generic_Association, Association_List);
            exit when not Comma_Present;
         end loop;

         T_Right_Paren;
         return Association_List;
      end if;

   end P_Generic_Actual_Part_Opt;

   -------------------------------
   -- 12.3  Generic Association --
   -------------------------------

   --  GENERIC_ASSOCIATION ::=
   --    [generic_formal_parameter_SELECTOR_NAME =>]
   --      EXPLICIT_GENERIC_ACTUAL_PARAMETER

   --  EXPLICIT_GENERIC_ACTUAL_PARAMETER ::=
   --    EXPRESSION      | variable_NAME   | subprogram_NAME
   --  | entry_NAME      | SUBTYPE_MARK    | package_instance_NAME

   --  Error recovery: cannot raise Error_Resync

   function P_Generic_Association return Node_Id is
      Scan_State         : Saved_Scan_State;
      Param_Name_Node    : Node_Id;
      Generic_Assoc_Node : Node_Id;

   begin
      Generic_Assoc_Node := New_Node (N_Generic_Association, Token_Ptr);

      --  Ada 2005: an association can be given by: others => <>

      if Token = Tok_Others then
         if Ada_Version < Ada_2005 then
            Error_Msg_SP
              ("partial parametrization of formal packages" &
                " is an Ada 2005 extension");
            Error_Msg_SP
              ("\unit must be compiled with -gnat05 switch");
         end if;

         Scan;  --  past OTHERS

         if Token /= Tok_Arrow then
            Error_Msg_BC  ("expect arrow after others");
         else
            Scan;  --  past arrow
         end if;

         if Token /= Tok_Box then
            Error_Msg_BC ("expect Box after arrow");
         else
            Scan;  --  past box
         end if;

         --  Source position of the others choice is beginning of construct

         return New_Node (N_Others_Choice, Sloc (Generic_Assoc_Node));
      end if;

      if Token in Token_Class_Desig then
         Param_Name_Node := Token_Node;
         Save_Scan_State (Scan_State); -- at designator
         Scan; -- past simple name or operator symbol

         if Token = Tok_Arrow then
            Scan; -- past arrow
            Set_Selector_Name (Generic_Assoc_Node, Param_Name_Node);
         else
            Restore_Scan_State (Scan_State); -- to designator
         end if;
      end if;

      --  In Ada 2005 the actual can be a box

      if Token = Tok_Box then
         Scan;
         Set_Box_Present (Generic_Assoc_Node);
         Set_Explicit_Generic_Actual_Parameter (Generic_Assoc_Node, Empty);

      else
         Set_Explicit_Generic_Actual_Parameter
           (Generic_Assoc_Node, P_Expression);
      end if;

      return Generic_Assoc_Node;
   end P_Generic_Association;

   ---------------------------------------------
   -- 12.3  Explicit Generic Actual Parameter --
   ---------------------------------------------

   --  Parsed by P_Generic_Association (12.3)

   --------------------------------------
   -- 12.4  Formal Object Declarations --
   --------------------------------------

   --  FORMAL_OBJECT_DECLARATION ::=
   --    DEFINING_IDENTIFIER_LIST :
   --      MODE [NULL_EXCLUSION] SUBTYPE_MARK [:= DEFAULT_EXPRESSION]
   --        [ASPECT_SPECIFICATIONS];
   --  | DEFINING_IDENTIFIER_LIST :
   --      MODE ACCESS_DEFINITION [:= DEFAULT_EXPRESSION];
   --        [ASPECT_SPECIFICATIONS];

   --  The caller has checked that the initial token is an identifier

   --  Error recovery: cannot raise Error_Resync

   procedure P_Formal_Object_Declarations (Decls : List_Id) is
      Decl_Node        : Node_Id;
      Ident            : Nat;
      Not_Null_Present : Boolean := False;
      Num_Idents       : Nat;
      Scan_State       : Saved_Scan_State;

      Idents : array (Int range 1 .. 4096) of Entity_Id;
      --  This array holds the list of defining identifiers. The upper bound
      --  of 4096 is intended to be essentially infinite, and we do not even
      --  bother to check for it being exceeded.

   begin
      Idents (1) := P_Defining_Identifier (C_Comma_Colon);
      Num_Idents := 1;
      while Comma_Present loop
         Num_Idents := Num_Idents + 1;
         Idents (Num_Idents) := P_Defining_Identifier (C_Comma_Colon);
      end loop;

      T_Colon;

      --  If there are multiple identifiers, we repeatedly scan the
      --  type and initialization expression information by resetting
      --  the scan pointer (so that we get completely separate trees
      --  for each occurrence).

      if Num_Idents > 1 then
         Save_Scan_State (Scan_State);
      end if;

      --  Loop through defining identifiers in list

      Ident := 1;
      Ident_Loop : loop
         Decl_Node := New_Node (N_Formal_Object_Declaration, Token_Ptr);
         Set_Defining_Identifier (Decl_Node, Idents (Ident));
         P_Mode (Decl_Node);

         Not_Null_Present := P_Null_Exclusion;  --  Ada 2005 (AI-423)

         --  Ada 2005 (AI-423): Formal object with an access definition

         if Token = Tok_Access then

            --  The access definition is still parsed and set even though
            --  the compilation may not use the proper switch. This action
            --  ensures the required local error recovery.

            Set_Access_Definition (Decl_Node,
              P_Access_Definition (Not_Null_Present));

            if Ada_Version < Ada_2005 then
               Error_Msg_SP
                 ("access definition not allowed in formal object " &
                  "declaration");
               Error_Msg_SP ("\unit must be compiled with -gnat05 switch");
            end if;

         --  Formal object with a subtype mark

         else
            Set_Null_Exclusion_Present (Decl_Node, Not_Null_Present);
            Set_Subtype_Mark (Decl_Node, P_Subtype_Mark_Resync);
         end if;

         No_Constraint;
         Set_Default_Expression (Decl_Node, Init_Expr_Opt);
         P_Aspect_Specifications (Decl_Node);

         if Ident > 1 then
            Set_Prev_Ids (Decl_Node, True);
         end if;

         if Ident < Num_Idents then
            Set_More_Ids (Decl_Node, True);
         end if;

         Append (Decl_Node, Decls);

         exit Ident_Loop when Ident = Num_Idents;
         Ident := Ident + 1;
         Restore_Scan_State (Scan_State);
      end loop Ident_Loop;
   end P_Formal_Object_Declarations;

   -----------------------------------
   -- 12.5  Formal Type Declaration --
   -----------------------------------

   --  FORMAL_TYPE_DECLARATION ::=
   --    type DEFINING_IDENTIFIER [DISCRIMINANT_PART]
   --      is FORMAL_TYPE_DEFINITION
   --        [ASPECT_SPECIFICATIONS];

   --  The caller has checked that the initial token is TYPE

   --  Error recovery: cannot raise Error_Resync

   function P_Formal_Type_Declaration return Node_Id is
      Decl_Node  : Node_Id;
      Def_Node   : Node_Id;

   begin
      Decl_Node := New_Node (N_Formal_Type_Declaration, Token_Ptr);
      Scan; -- past TYPE
      Set_Defining_Identifier (Decl_Node, P_Defining_Identifier);

      if P_Unknown_Discriminant_Part_Opt then
         Set_Unknown_Discriminants_Present (Decl_Node, True);
      else
         Set_Discriminant_Specifications
           (Decl_Node, P_Known_Discriminant_Part_Opt);
      end if;

      if Token = Tok_Semicolon then

         --  Ada 2012: Incomplete formal type

         Scan; -- past semicolon

         if Ada_Version < Ada_2012 then
            Error_Msg_N
              ("`formal incomplete type` is an Ada 2012 feature", Decl_Node);
            Error_Msg_N
              ("\unit must be compiled with -gnat2012 switch", Decl_Node);
         end if;

         Set_Formal_Type_Definition
           (Decl_Node,
            New_Node (N_Formal_Incomplete_Type_Definition, Token_Ptr));
         return Decl_Node;

      else
         T_Is;
      end if;

      Def_Node := P_Formal_Type_Definition;

      if Nkind (Def_Node) = N_Formal_Incomplete_Type_Definition
        and then Ada_Version < Ada_2012
      then
         Error_Msg_N
           ("`formal incomplete type` is an Ada 2012 feature", Decl_Node);
         Error_Msg_N
           ("\unit must be compiled with -gnat2012 switch", Decl_Node);
      end if;

      if Def_Node /= Error then
         Set_Formal_Type_Definition (Decl_Node, Def_Node);
         P_Aspect_Specifications (Decl_Node);

      else
         Decl_Node := Error;

         --  If we have aspect specifications, skip them

         if Aspect_Specifications_Present then
            P_Aspect_Specifications (Error);

         --  If we have semicolon, skip it to avoid cascaded errors

         elsif Token = Tok_Semicolon then
            Scan; -- past semicolon
         end if;
      end if;

      return Decl_Node;
   end P_Formal_Type_Declaration;

   ----------------------------------
   -- 12.5  Formal Type Definition --
   ----------------------------------

   --  FORMAL_TYPE_DEFINITION ::=
   --    FORMAL_PRIVATE_TYPE_DEFINITION
   --  | FORMAL_INCOMPLETE_TYPE_DEFINITION
   --  | FORMAL_DERIVED_TYPE_DEFINITION
   --  | FORMAL_DISCRETE_TYPE_DEFINITION
   --  | FORMAL_SIGNED_INTEGER_TYPE_DEFINITION
   --  | FORMAL_MODULAR_TYPE_DEFINITION
   --  | FORMAL_FLOATING_POINT_DEFINITION
   --  | FORMAL_ORDINARY_FIXED_POINT_DEFINITION
   --  | FORMAL_DECIMAL_FIXED_POINT_DEFINITION
   --  | FORMAL_ARRAY_TYPE_DEFINITION
   --  | FORMAL_ACCESS_TYPE_DEFINITION
   --  | FORMAL_INTERFACE_TYPE_DEFINITION

   --  FORMAL_ARRAY_TYPE_DEFINITION ::= ARRAY_TYPE_DEFINITION

   --  FORMAL_ACCESS_TYPE_DEFINITION ::= ACCESS_TYPE_DEFINITION

   --  FORMAL_INTERFACE_TYPE_DEFINITION ::= INTERFACE_TYPE_DEFINITION

   function P_Formal_Type_Definition return Node_Id is
      Scan_State   : Saved_Scan_State;
      Typedef_Node : Node_Id;

   begin
      if Token_Name = Name_Abstract then
         Check_95_Keyword (Tok_Abstract, Tok_Tagged);
      end if;

      if Token_Name = Name_Tagged then
         Check_95_Keyword (Tok_Tagged, Tok_Private);
         Check_95_Keyword (Tok_Tagged, Tok_Limited);
      end if;

      case Token is

         --  Mostly we can tell what we have from the initial token. The one
         --  exception is ABSTRACT, where we have to scan ahead to see if we
         --  have a formal derived type or a formal private type definition.

         --  In addition, in Ada 2005 LIMITED may appear after abstract, so
         --  that the lookahead must be extended by one more token.

         when Tok_Abstract =>
            Save_Scan_State (Scan_State);
            Scan; -- past ABSTRACT

            if Token = Tok_New then
               Restore_Scan_State (Scan_State); -- to ABSTRACT
               return P_Formal_Derived_Type_Definition;

            elsif Token = Tok_Limited then
               Scan;  --  past LIMITED

               if Token = Tok_New then
                  Restore_Scan_State (Scan_State); -- to ABSTRACT
                  return P_Formal_Derived_Type_Definition;

               else
                  Restore_Scan_State (Scan_State); -- to ABSTRACT
                  return P_Formal_Private_Type_Definition;
               end if;

            --  Ada 2005 (AI-443): Abstract synchronized formal derived type

            elsif Token = Tok_Synchronized then
               Restore_Scan_State (Scan_State); -- to ABSTRACT
               return P_Formal_Derived_Type_Definition;

            else
               Restore_Scan_State (Scan_State); -- to ABSTRACT
               return P_Formal_Private_Type_Definition;
            end if;

         when Tok_Access =>
            return P_Access_Type_Definition;

         when Tok_Array =>
            return P_Array_Type_Definition;

         when Tok_Delta =>
            return P_Formal_Fixed_Point_Definition;

         when Tok_Digits =>
            return P_Formal_Floating_Point_Definition;

         when Tok_Interface => --  Ada 2005 (AI-251)
            return P_Interface_Type_Definition (Abstract_Present => False);

         when Tok_Left_Paren =>
            return P_Formal_Discrete_Type_Definition;

         when Tok_Limited =>
            Save_Scan_State (Scan_State);
            Scan; --  past LIMITED

            if Token = Tok_Interface then
               Typedef_Node :=
                 P_Interface_Type_Definition (Abstract_Present => False);
               Set_Limited_Present (Typedef_Node);
               return Typedef_Node;

            elsif Token = Tok_New then
               Restore_Scan_State (Scan_State); -- to LIMITED
               return P_Formal_Derived_Type_Definition;

            else
               if Token = Tok_Abstract then
                  Error_Msg_SC -- CODEFIX
                    ("ABSTRACT must come before LIMITED");
                  Scan;  --  past improper ABSTRACT

                  if Token = Tok_New then
                     Restore_Scan_State (Scan_State); -- to LIMITED
                     return P_Formal_Derived_Type_Definition;

                  else
                     Restore_Scan_State (Scan_State);
                     return P_Formal_Private_Type_Definition;
                  end if;
               end if;

               Restore_Scan_State (Scan_State);
               return P_Formal_Private_Type_Definition;
            end if;

         when Tok_Mod =>
            return P_Formal_Modular_Type_Definition;

         when Tok_New =>
            return P_Formal_Derived_Type_Definition;

         when Tok_Not =>
            if P_Null_Exclusion then
               Typedef_Node :=  P_Access_Type_Definition;
               Set_Null_Exclusion_Present (Typedef_Node);
               return Typedef_Node;

            else
               Error_Msg_SC ("expect valid formal access definition!");
               Resync_Past_Semicolon;
               return Error;
            end if;

         when Tok_Private  =>
            return P_Formal_Private_Type_Definition;

         when  Tok_Tagged  =>
            if Next_Token_Is (Tok_Semicolon) then
               Typedef_Node :=
                 New_Node (N_Formal_Incomplete_Type_Definition, Token_Ptr);
               Set_Tagged_Present (Typedef_Node);

               Scan;  --  past tagged
               return Typedef_Node;

            else
               return P_Formal_Private_Type_Definition;
            end if;

         when Tok_Range =>
            return P_Formal_Signed_Integer_Type_Definition;

         when Tok_Record =>
            Error_Msg_SC ("record not allowed in generic type definition!");
            Discard_Junk_Node (P_Record_Definition);
            return Error;

         --  Ada 2005 (AI-345): Task, Protected or Synchronized interface or
         --  (AI-443): Synchronized formal derived type declaration.

         when Tok_Protected    |
              Tok_Synchronized |
              Tok_Task         =>

            declare
               Saved_Token : constant Token_Type := Token;

            begin
               Scan; -- past TASK, PROTECTED or SYNCHRONIZED

               --  Synchronized derived type

               if Token = Tok_New then
                  Typedef_Node := P_Formal_Derived_Type_Definition;

                  if Saved_Token = Tok_Synchronized then
                     Set_Synchronized_Present (Typedef_Node);
                  else
                     Error_Msg_SC ("invalid kind of formal derived type");
                  end if;

               --  Interface

               else
                  Typedef_Node :=
                    P_Interface_Type_Definition (Abstract_Present => False);

                  case Saved_Token is
                     when Tok_Task =>
                        Set_Task_Present         (Typedef_Node);

                     when Tok_Protected =>
                        Set_Protected_Present    (Typedef_Node);

                     when Tok_Synchronized =>
                        Set_Synchronized_Present (Typedef_Node);

                     when others =>
                        null;
                  end case;
               end if;

               return Typedef_Node;
            end;

         when others =>
            Error_Msg_BC ("expecting generic type definition here");
            Resync_Past_Semicolon;
            return Error;

      end case;
   end P_Formal_Type_Definition;

   --------------------------------------------
   -- 12.5.1  Formal Private Type Definition --
   --------------------------------------------

   --  FORMAL_PRIVATE_TYPE_DEFINITION ::=
   --    [[abstract] tagged] [limited] private

   --  The caller has checked the initial token is PRIVATE, ABSTRACT,
   --   TAGGED or LIMITED

   --  Error recovery: cannot raise Error_Resync

   function P_Formal_Private_Type_Definition return Node_Id is
      Def_Node : Node_Id;

   begin
      Def_Node := New_Node (N_Formal_Private_Type_Definition, Token_Ptr);

      if Token = Tok_Abstract then
         Scan; -- past ABSTRACT

         if Token_Name = Name_Tagged then
            Check_95_Keyword (Tok_Tagged, Tok_Private);
            Check_95_Keyword (Tok_Tagged, Tok_Limited);
         end if;

         if Token /= Tok_Tagged then
            Error_Msg_SP ("ABSTRACT must be followed by TAGGED");
         else
            Set_Abstract_Present (Def_Node, True);
         end if;
      end if;

      if Token = Tok_Tagged then
         Set_Tagged_Present (Def_Node, True);
         Scan; -- past TAGGED
      end if;

      if Token = Tok_Limited then
         Set_Limited_Present (Def_Node, True);
         Scan; -- past LIMITED
      end if;

      if Token = Tok_Abstract then
         if Prev_Token = Tok_Tagged then
            Error_Msg_SC -- CODEFIX
              ("ABSTRACT must come before TAGGED");
         elsif Prev_Token = Tok_Limited then
            Error_Msg_SC -- CODEFIX
              ("ABSTRACT must come before LIMITED");
         end if;

         Resync_Past_Semicolon;

      elsif Token = Tok_Tagged then
         Error_Msg_SC -- CODEFIX
           ("TAGGED must come before LIMITED");
         Resync_Past_Semicolon;
      end if;

      Set_Sloc (Def_Node, Token_Ptr);
      T_Private;

      if Token = Tok_Tagged then -- CODEFIX
         Error_Msg_SC ("TAGGED must come before PRIVATE");
         Scan; -- past TAGGED

      elsif Token = Tok_Abstract then -- CODEFIX
         Error_Msg_SC ("`ABSTRACT TAGGED` must come before PRIVATE");
         Scan; -- past ABSTRACT

         if Token = Tok_Tagged then
            Scan; -- past TAGGED
         end if;
      end if;

      return Def_Node;
   end P_Formal_Private_Type_Definition;

   --------------------------------------------
   -- 12.5.1  Formal Derived Type Definition --
   --------------------------------------------

   --  FORMAL_DERIVED_TYPE_DEFINITION ::=
   --    [abstract] [limited | synchronized]
   --         new SUBTYPE_MARK [[and INTERFACE_LIST] with private]

   --  The caller has checked the initial token(s) is/are NEW, ABSTRACT NEW,
   --  or LIMITED NEW, ABSTRACT LIMITED NEW, SYNCHRONIZED NEW or ABSTRACT
   --  SYNCHRONIZED NEW.

   --  Error recovery: cannot raise Error_Resync

   function P_Formal_Derived_Type_Definition return Node_Id is
      Def_Node : Node_Id;

   begin
      Def_Node := New_Node (N_Formal_Derived_Type_Definition, Token_Ptr);

      if Token = Tok_Abstract then
         Set_Abstract_Present (Def_Node);
         Scan; -- past ABSTRACT
      end if;

      if Token = Tok_Limited then
         Set_Limited_Present (Def_Node);
         Scan;  --  past LIMITED

         if Ada_Version < Ada_2005 then
            Error_Msg_SP
              ("LIMITED in derived type is an Ada 2005 extension");
            Error_Msg_SP
              ("\unit must be compiled with -gnat05 switch");
         end if;

      elsif Token = Tok_Synchronized then
         Set_Synchronized_Present (Def_Node);
         Scan;  --  past SYNCHRONIZED

         if Ada_Version < Ada_2005 then
            Error_Msg_SP
              ("SYNCHRONIZED in derived type is an Ada 2005 extension");
            Error_Msg_SP
              ("\unit must be compiled with -gnat05 switch");
         end if;
      end if;

      if Token = Tok_Abstract then
         Scan;  --  past ABSTRACT, diagnosed already in caller.
      end if;

      Scan; -- past NEW;
      Set_Subtype_Mark (Def_Node, P_Subtype_Mark);
      No_Constraint;

      --  Ada 2005 (AI-251): Deal with interfaces

      if Token = Tok_And then
         Scan; -- past AND

         if Ada_Version < Ada_2005 then
            Error_Msg_SP
              ("abstract interface is an Ada 2005 extension");
            Error_Msg_SP ("\unit must be compiled with -gnat05 switch");
         end if;

         Set_Interface_List (Def_Node, New_List);

         loop
            Append (P_Qualified_Simple_Name, Interface_List (Def_Node));
            exit when Token /= Tok_And;
            Scan; -- past AND
         end loop;
      end if;

      if Token = Tok_With then
         Scan; -- past WITH
         Set_Private_Present (Def_Node, True);
         T_Private;

      elsif Token = Tok_Tagged then
         Scan;

         if Token = Tok_Private then
            Error_Msg_SC  -- CODEFIX
              ("TAGGED should be WITH");
            Set_Private_Present (Def_Node, True);
            T_Private;
         else
            Ignore (Tok_Tagged);
         end if;
      end if;

      return Def_Node;
   end P_Formal_Derived_Type_Definition;

   ---------------------------------------------
   -- 12.5.2  Formal Discrete Type Definition --
   ---------------------------------------------

   --  FORMAL_DISCRETE_TYPE_DEFINITION ::= (<>)

   --  The caller has checked the initial token is left paren

   --  Error recovery: cannot raise Error_Resync

   function P_Formal_Discrete_Type_Definition return Node_Id is
      Def_Node : Node_Id;

   begin
      Def_Node := New_Node (N_Formal_Discrete_Type_Definition, Token_Ptr);
      Scan; -- past left paren
      T_Box;
      T_Right_Paren;
      return Def_Node;
   end P_Formal_Discrete_Type_Definition;

   ---------------------------------------------------
   -- 12.5.2  Formal Signed Integer Type Definition --
   ---------------------------------------------------

   --  FORMAL_SIGNED_INTEGER_TYPE_DEFINITION ::= range <>

   --  The caller has checked the initial token is RANGE

   --  Error recovery: cannot raise Error_Resync

   function P_Formal_Signed_Integer_Type_Definition return Node_Id is
      Def_Node : Node_Id;

   begin
      Def_Node :=
        New_Node (N_Formal_Signed_Integer_Type_Definition, Token_Ptr);
      Scan; -- past RANGE
      T_Box;
      return Def_Node;
   end P_Formal_Signed_Integer_Type_Definition;

   --------------------------------------------
   -- 12.5.2  Formal Modular Type Definition --
   --------------------------------------------

   --  FORMAL_MODULAR_TYPE_DEFINITION ::= mod <>

   --  The caller has checked the initial token is MOD

   --  Error recovery: cannot raise Error_Resync

   function P_Formal_Modular_Type_Definition return Node_Id is
      Def_Node : Node_Id;

   begin
      Def_Node :=
        New_Node (N_Formal_Modular_Type_Definition, Token_Ptr);
      Scan; -- past MOD
      T_Box;
      return Def_Node;
   end P_Formal_Modular_Type_Definition;

   ----------------------------------------------
   -- 12.5.2  Formal Floating Point Definition --
   ----------------------------------------------

   --  FORMAL_FLOATING_POINT_DEFINITION ::= digits <>

   --  The caller has checked the initial token is DIGITS

   --  Error recovery: cannot raise Error_Resync

   function P_Formal_Floating_Point_Definition return Node_Id is
      Def_Node : Node_Id;

   begin
      Def_Node :=
        New_Node (N_Formal_Floating_Point_Definition, Token_Ptr);
      Scan; -- past DIGITS
      T_Box;
      return Def_Node;
   end P_Formal_Floating_Point_Definition;

   -------------------------------------------
   -- 12.5.2  Formal Fixed Point Definition --
   -------------------------------------------

   --  This routine parses either a formal ordinary fixed point definition
   --  or a formal decimal fixed point definition:

   --  FORMAL_ORDINARY_FIXED_POINT_DEFINITION ::= delta <>

   --  FORMAL_DECIMAL_FIXED_POINT_DEFINITION ::= delta <> digits <>

   --  The caller has checked the initial token is DELTA

   --  Error recovery: cannot raise Error_Resync

   function P_Formal_Fixed_Point_Definition return Node_Id is
      Def_Node   : Node_Id;
      Delta_Sloc : Source_Ptr;

   begin
      Delta_Sloc := Token_Ptr;
      Scan; -- past DELTA
      T_Box;

      if Token = Tok_Digits then
         Def_Node :=
           New_Node (N_Formal_Decimal_Fixed_Point_Definition, Delta_Sloc);
         Scan; -- past DIGITS
         T_Box;
      else
         Def_Node :=
           New_Node (N_Formal_Ordinary_Fixed_Point_Definition, Delta_Sloc);
      end if;

      return Def_Node;
   end P_Formal_Fixed_Point_Definition;

   ----------------------------------------------------
   -- 12.5.2  Formal Ordinary Fixed Point Definition --
   ----------------------------------------------------

   --  Parsed by P_Formal_Fixed_Point_Definition (12.5.2)

   ---------------------------------------------------
   -- 12.5.2  Formal Decimal Fixed Point Definition --
   ---------------------------------------------------

   --  Parsed by P_Formal_Fixed_Point_Definition (12.5.2)

   ------------------------------------------
   -- 12.5.3  Formal Array Type Definition --
   ------------------------------------------

   --  Parsed by P_Formal_Type_Definition (12.5)

   -------------------------------------------
   -- 12.5.4  Formal Access Type Definition --
   -------------------------------------------

   --  Parsed by P_Formal_Type_Definition (12.5)

   -----------------------------------------
   -- 12.6  Formal Subprogram Declaration --
   -----------------------------------------

   --  FORMAL_SUBPROGRAM_DECLARATION ::=
   --    FORMAL_CONCRETE_SUBPROGRAM_DECLARATION
   --  | FORMAL_ABSTRACT_SUBPROGRAM_DECLARATION

   --  FORMAL_CONCRETE_SUBPROGRAM_DECLARATION ::=
   --    with SUBPROGRAM_SPECIFICATION [is SUBPROGRAM_DEFAULT]
   --      [ASPECT_SPECIFICATIONS];

   --  FORMAL_ABSTRACT_SUBPROGRAM_DECLARATION ::=
   --    with SUBPROGRAM_SPECIFICATION is abstract [SUBPROGRAM_DEFAULT]
   --      [ASPECT_SPECIFICATIONS];

   --  SUBPROGRAM_DEFAULT ::= DEFAULT_NAME | <>

   --  DEFAULT_NAME ::= NAME | null

   --  The caller has checked that the initial tokens are WITH FUNCTION or
   --  WITH PROCEDURE, and the initial WITH has been scanned out.

   --  A null default is an Ada 2005 feature

   --  Error recovery: cannot raise Error_Resync

   function P_Formal_Subprogram_Declaration return Node_Id is
      Prev_Sloc : constant Source_Ptr := Prev_Token_Ptr;
      Spec_Node : constant Node_Id    := P_Subprogram_Specification;
      Def_Node  : Node_Id;

   begin
      if Token = Tok_Is then
         T_Is; -- past IS, skip extra IS or ";"

         if Token = Tok_Abstract then
            Def_Node :=
              New_Node (N_Formal_Abstract_Subprogram_Declaration, Prev_Sloc);
            Scan; -- past ABSTRACT

            if Ada_Version < Ada_2005 then
               Error_Msg_SP
                 ("formal abstract subprograms are an Ada 2005 extension");
               Error_Msg_SP ("\unit must be compiled with -gnat05 switch");
            end if;

         else
            Def_Node :=
              New_Node (N_Formal_Concrete_Subprogram_Declaration, Prev_Sloc);
         end if;

         Set_Specification (Def_Node, Spec_Node);

         if Token = Tok_Semicolon then
            null;

         elsif Aspect_Specifications_Present then
            null;

         elsif Token = Tok_Box then
            Set_Box_Present (Def_Node, True);
            Scan; -- past <>

         elsif Token = Tok_Null then
            if Ada_Version < Ada_2005 then
               Error_Msg_SP
                 ("null default subprograms are an Ada 2005 extension");
               Error_Msg_SP ("\unit must be compiled with -gnat05 switch");
            end if;

            if Nkind (Spec_Node) = N_Procedure_Specification then
               Set_Null_Present (Spec_Node);
            else
               Error_Msg_SP ("only procedures can be null");
            end if;

            Scan;  --  past NULL

         else
            Set_Default_Name (Def_Node, P_Name);
         end if;

      else
         Def_Node :=
           New_Node (N_Formal_Concrete_Subprogram_Declaration, Prev_Sloc);
         Set_Specification (Def_Node, Spec_Node);
      end if;

      P_Aspect_Specifications (Def_Node);
      return Def_Node;
   end P_Formal_Subprogram_Declaration;

   ------------------------------
   -- 12.6  Subprogram Default --
   ------------------------------

   --  Parsed by P_Formal_Procedure_Declaration (12.6)

   ------------------------
   -- 12.6  Default Name --
   ------------------------

   --  Parsed by P_Formal_Procedure_Declaration (12.6)

   --------------------------------------
   -- 12.7  Formal Package Declaration --
   --------------------------------------

   --  FORMAL_PACKAGE_DECLARATION ::=
   --    with package DEFINING_IDENTIFIER
   --      is new generic_package_NAME FORMAL_PACKAGE_ACTUAL_PART
   --        [ASPECT_SPECIFICATIONS];

   --  FORMAL_PACKAGE_ACTUAL_PART ::=
   --    ([OTHERS =>] <>) |
   --    [GENERIC_ACTUAL_PART]
   --    (FORMAL_PACKAGE_ASSOCIATION {, FORMAL_PACKAGE_ASSOCIATION}
   --      [, OTHERS => <>)

   --  FORMAL_PACKAGE_ASSOCIATION ::=
   --    GENERIC_ASSOCIATION
   --    | GENERIC_FORMAL_PARAMETER_SELECTOR_NAME => <>

   --  The caller has checked that the initial tokens are WITH PACKAGE,
   --  and the initial WITH has been scanned out (so Token = Tok_Package).

   --  Error recovery: cannot raise Error_Resync

   function P_Formal_Package_Declaration return Node_Id is
      Def_Node : Node_Id;
      Scan_State : Saved_Scan_State;

   begin
      Def_Node := New_Node (N_Formal_Package_Declaration, Prev_Token_Ptr);
      Scan; -- past PACKAGE
      Set_Defining_Identifier (Def_Node, P_Defining_Identifier (C_Is));
      T_Is;
      T_New;
      Set_Name (Def_Node, P_Qualified_Simple_Name);

      if Token = Tok_Left_Paren then
         Save_Scan_State (Scan_State); -- at the left paren
         Scan; -- past the left paren

         if Token = Tok_Box then
            Set_Box_Present (Def_Node, True);
            Scan; -- past box
            T_Right_Paren;

         else
            Restore_Scan_State (Scan_State); -- to the left paren
            Set_Generic_Associations (Def_Node, P_Generic_Actual_Part_Opt);
         end if;
      end if;

      P_Aspect_Specifications (Def_Node);
      return Def_Node;
   end P_Formal_Package_Declaration;

   --------------------------------------
   -- 12.7  Formal Package Actual Part --
   --------------------------------------

   --  Parsed by P_Formal_Package_Declaration (12.7)

end Ch12;
