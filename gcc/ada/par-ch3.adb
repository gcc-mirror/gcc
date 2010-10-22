------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P A R . C H 3                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2010, Free Software Foundation, Inc.         --
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
--  by RM section rather than alphabetical.

with Sinfo.CN; use Sinfo.CN;

separate (Par)

---------
-- Ch3 --
---------

package body Ch3 is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function P_Component_List                               return Node_Id;
   function P_Defining_Character_Literal                   return Node_Id;
   function P_Delta_Constraint                             return Node_Id;
   function P_Derived_Type_Def_Or_Private_Ext_Decl         return Node_Id;
   function P_Digits_Constraint                            return Node_Id;
   function P_Discriminant_Association                     return Node_Id;
   function P_Enumeration_Literal_Specification            return Node_Id;
   function P_Enumeration_Type_Definition                  return Node_Id;
   function P_Fixed_Point_Definition                       return Node_Id;
   function P_Floating_Point_Definition                    return Node_Id;
   function P_Index_Or_Discriminant_Constraint             return Node_Id;
   function P_Real_Range_Specification_Opt                 return Node_Id;
   function P_Subtype_Declaration                          return Node_Id;
   function P_Type_Declaration                             return Node_Id;
   function P_Modular_Type_Definition                      return Node_Id;
   function P_Variant                                      return Node_Id;
   function P_Variant_Part                                 return Node_Id;

   procedure Check_Restricted_Expression (N : Node_Id);
   --  Check that the expression N meets the Restricted_Expression syntax.
   --  The syntax is as follows:
   --
   --    RESTRICTED_EXPRESSION ::=
   --        RESTRICTED_RELATION {and RESTRICTED_RELATION}
   --      | RESTRICTED_RELATION {and then RESTRICTED_RELATION}
   --      | RESTRICTED_RELATION {or RESTRICTED_RELATION}
   --      | RESTRICTED_RELATION {or else RESTRICTED_RELATION}
   --      | RESTRICTED_RELATION {xor RESTRICTED_RELATION}
   --
   --    RESTRICTED_RELATION ::=
   --       SIMPLE_EXPRESSION [RELATIONAL_OPERATOR SIMPLE_EXPRESSION]
   --
   --  This syntax is used for choices when extensions (and set notations)
   --  are enabled, to remove the ambiguity of "when X in A | B". We consider
   --  it very unlikely that this will ever arise in practice.

   procedure P_Declarative_Items
     (Decls   : List_Id;
      Done    : out Boolean;
      In_Spec : Boolean);
   --  Scans out a single declarative item, or, in the case of a declaration
   --  with a list of identifiers, a list of declarations, one for each of the
   --  identifiers in the list. The declaration or declarations scanned are
   --  appended to the given list. Done indicates whether or not there may be
   --  additional declarative items to scan. If Done is True, then a decision
   --  has been made that there are no more items to scan. If Done is False,
   --  then there may be additional declarations to scan. In_Spec is true if
   --  we are scanning a package declaration, and is used to generate an
   --  appropriate message if a statement is encountered in such a context.

   procedure P_Identifier_Declarations
     (Decls   : List_Id;
      Done    : out Boolean;
      In_Spec : Boolean);
   --  Scans out a set of declarations for an identifier or list of
   --  identifiers, and appends them to the given list. The parameters have
   --  the same significance as for P_Declarative_Items.

   procedure Statement_When_Declaration_Expected
     (Decls   : List_Id;
      Done    : out Boolean;
      In_Spec : Boolean);
   --  Called when a statement is found at a point where a declaration was
   --  expected. The parameters are as described for P_Declarative_Items.

   procedure Set_Declaration_Expected;
   --  Posts a "declaration expected" error messages at the start of the
   --  current token, and if this is the first such message issued, saves
   --  the message id in Missing_Begin_Msg, for possible later replacement.

   ---------------------------------
   -- Check_Restricted_Expression --
   ---------------------------------

   procedure Check_Restricted_Expression (N : Node_Id) is
   begin
      if Nkind_In (N, N_Op_And, N_Op_Or, N_Op_Xor, N_And_Then, N_Or_Else) then
         Check_Restricted_Expression (Left_Opnd (N));
         Check_Restricted_Expression (Right_Opnd (N));

      elsif Nkind_In (N, N_In, N_Not_In)
        and then Paren_Count (N) = 0
      then
         Error_Msg_N ("|this expression must be parenthesized!", N);
      end if;
   end Check_Restricted_Expression;

   -------------------
   -- Init_Expr_Opt --
   -------------------

   function Init_Expr_Opt (P : Boolean := False) return Node_Id is
   begin
      --  For colon, assume it means := unless it is at the end of
      --  a line, in which case guess that it means a semicolon.

      if Token = Tok_Colon then
         if Token_Is_At_End_Of_Line then
            T_Semicolon;
            return Empty;
         end if;

      --  Here if := or something that we will take as equivalent

      elsif Token = Tok_Colon_Equal
        or else Token = Tok_Equal
        or else Token = Tok_Is
      then
         null;

      --  Another possibility. If we have a literal followed by a semicolon,
      --  we assume that we have a missing colon-equal.

      elsif Token in Token_Class_Literal then
         declare
            Scan_State : Saved_Scan_State;

         begin
            Save_Scan_State (Scan_State);
            Scan; -- past literal or identifier

            if Token = Tok_Semicolon then
               Restore_Scan_State (Scan_State);
            else
               Restore_Scan_State (Scan_State);
               return Empty;
            end if;
         end;

      --  Otherwise we definitely have no initialization expression

      else
         return Empty;
      end if;

      --  Merge here if we have an initialization expression

      T_Colon_Equal;

      if P then
         return P_Expression;
      else
         return P_Expression_No_Right_Paren;
      end if;
   end Init_Expr_Opt;

   ----------------------------
   -- 3.1  Basic Declaration --
   ----------------------------

   --  Parsed by P_Basic_Declarative_Items (3.9)

   ------------------------------
   -- 3.1  Defining Identifier --
   ------------------------------

   --  DEFINING_IDENTIFIER ::= IDENTIFIER

   --  Error recovery: can raise Error_Resync

   function P_Defining_Identifier (C : Id_Check := None) return Node_Id is
      Ident_Node : Node_Id;

   begin
      --  Scan out the identifier. Note that this code is essentially identical
      --  to P_Identifier, except that in the call to Scan_Reserved_Identifier
      --  we set Force_Msg to True, since we want at least one message for each
      --  separate declaration (but not use) of a reserved identifier.

      if Token = Tok_Identifier then

         --  Ada 2005 (AI-284): Compiling in Ada95 mode we warn that INTERFACE,
         --  OVERRIDING, and SYNCHRONIZED are new reserved words. Note that
         --  in the case where these keywords are misused in Ada 95 mode,
         --  this routine will generally not be called at all.

         if Ada_Version = Ada_95
           and then Warn_On_Ada_2005_Compatibility
         then
            if Token_Name = Name_Overriding
              or else Token_Name = Name_Synchronized
              or else (Token_Name = Name_Interface
                        and then Prev_Token /= Tok_Pragma)
            then
               Error_Msg_N ("& is a reserved word in Ada 2005?", Token_Node);
            end if;
         end if;

      --  If we have a reserved identifier, manufacture an identifier with
      --  a corresponding name after posting an appropriate error message

      elsif Is_Reserved_Identifier (C) then
         Scan_Reserved_Identifier (Force_Msg => True);

      --  Otherwise we have junk that cannot be interpreted as an identifier

      else
         T_Identifier; -- to give message
         raise Error_Resync;
      end if;

      Ident_Node := Token_Node;
      Scan; -- past the reserved identifier

      --  If we already have a defining identifier, clean it out and make
      --  a new clean identifier. This situation arises in some error cases
      --  and we need to fix it.

      if Nkind (Ident_Node) = N_Defining_Identifier then
         Ident_Node :=
           Make_Identifier (Sloc (Ident_Node),
             Chars => Chars (Ident_Node));
      end if;

      --  Change identifier to defining identifier if not in error

      if Ident_Node /= Error then
         Change_Identifier_To_Defining_Identifier (Ident_Node);
      end if;

      return Ident_Node;
   end P_Defining_Identifier;

   -----------------------------
   -- 3.2.1  Type Declaration --
   -----------------------------

   --  TYPE_DECLARATION ::=
   --    FULL_TYPE_DECLARATION
   --  | INCOMPLETE_TYPE_DECLARATION
   --  | PRIVATE_TYPE_DECLARATION
   --  | PRIVATE_EXTENSION_DECLARATION

   --  FULL_TYPE_DECLARATION ::=
   --    type DEFINING_IDENTIFIER [KNOWN_DISCRIMINANT_PART] is TYPE_DEFINITION
   --      [ASPECT_SPECIFICATIONS];
   --  | CONCURRENT_TYPE_DECLARATION

   --  INCOMPLETE_TYPE_DECLARATION ::=
   --    type DEFINING_IDENTIFIER [DISCRIMINANT_PART] [is tagged];

   --  PRIVATE_TYPE_DECLARATION ::=
   --    type DEFINING_IDENTIFIER [DISCRIMINANT_PART]
   --      is [abstract] [tagged] [limited] private;

   --  PRIVATE_EXTENSION_DECLARATION ::=
   --    type DEFINING_IDENTIFIER [DISCRIMINANT_PART] is
   --      [abstract] [limited | synchronized]
   --        new ancestor_SUBTYPE_INDICATION [and INTERFACE_LIST]
   --          with private;

   --  TYPE_DEFINITION ::=
   --    ENUMERATION_TYPE_DEFINITION  | INTEGER_TYPE_DEFINITION
   --  | REAL_TYPE_DEFINITION         | ARRAY_TYPE_DEFINITION
   --  | RECORD_TYPE_DEFINITION       | ACCESS_TYPE_DEFINITION
   --  | DERIVED_TYPE_DEFINITION      | INTERFACE_TYPE_DEFINITION

   --  INTEGER_TYPE_DEFINITION ::=
   --    SIGNED_INTEGER_TYPE_DEFINITION
   --    MODULAR_TYPE_DEFINITION

   --  INTERFACE_TYPE_DEFINITION ::=
   --    [limited | task | protected | synchronized ] interface
   --      [and INTERFACE_LIST]

   --  Error recovery: can raise Error_Resync

   --  The processing for full type declarations, incomplete type declarations,
   --  private type declarations and type definitions is included in this
   --  function. The processing for concurrent type declarations is NOT here,
   --  but rather in chapter 9 (this function handles only declarations
   --  starting with TYPE).

   function P_Type_Declaration return Node_Id is
      Abstract_Present : Boolean := False;
      Abstract_Loc     : Source_Ptr := No_Location;
      Decl_Node        : Node_Id;
      Discr_List       : List_Id;
      Discr_Sloc       : Source_Ptr;
      End_Labl         : Node_Id;
      Ident_Node       : Node_Id;
      Is_Derived_Iface : Boolean := False;
      Type_Loc         : Source_Ptr;
      Type_Start_Col   : Column_Number;
      Unknown_Dis      : Boolean;

      Typedef_Node : Node_Id;
      --  Normally holds type definition, except in the case of a private
      --  extension declaration, in which case it holds the declaration itself

   begin
      Type_Loc := Token_Ptr;
      Type_Start_Col := Start_Column;

      --  If we have TYPE, then proceed ahead and scan identifier

      if Token = Tok_Type then
         Type_Token_Location := Type_Loc;
         Scan; -- past TYPE
         Ident_Node := P_Defining_Identifier (C_Is);

      --  Otherwise this is an error case

      else
         T_Type;
         Type_Token_Location := Type_Loc;
         Ident_Node := P_Defining_Identifier (C_Is);
      end if;

      Discr_Sloc := Token_Ptr;

      if P_Unknown_Discriminant_Part_Opt then
         Unknown_Dis := True;
         Discr_List := No_List;
      else
         Unknown_Dis := False;
         Discr_List := P_Known_Discriminant_Part_Opt;
      end if;

      --  Incomplete type declaration. We complete the processing for this
      --  case here and return the resulting incomplete type declaration node

      if Token = Tok_Semicolon then
         Scan; -- past ;
         Decl_Node := New_Node (N_Incomplete_Type_Declaration, Type_Loc);
         Set_Defining_Identifier (Decl_Node, Ident_Node);
         Set_Unknown_Discriminants_Present (Decl_Node, Unknown_Dis);
         Set_Discriminant_Specifications (Decl_Node, Discr_List);
         return Decl_Node;

      else
         Decl_Node := Empty;
      end if;

      --  Full type declaration or private type declaration, must have IS

      if Token = Tok_Equal then
         TF_Is;
         Scan; -- past = used in place of IS

      elsif Token = Tok_Renames then
         Error_Msg_SC  -- CODEFIX
           ("RENAMES should be IS");
         Scan; -- past RENAMES used in place of IS

      else
         TF_Is;
      end if;

      --  First an error check, if we have two identifiers in a row, a likely
      --  possibility is that the first of the identifiers is an incorrectly
      --  spelled keyword.

      if Token = Tok_Identifier then
         declare
            SS : Saved_Scan_State;
            I2 : Boolean;

         begin
            Save_Scan_State (SS);
            Scan; -- past initial identifier
            I2 := (Token = Tok_Identifier);
            Restore_Scan_State (SS);

            if I2
              and then
                (Bad_Spelling_Of (Tok_Abstract) or else
                 Bad_Spelling_Of (Tok_Access)   or else
                 Bad_Spelling_Of (Tok_Aliased)  or else
                 Bad_Spelling_Of (Tok_Constant))
            then
               null;
            end if;
         end;
      end if;

      --  Check for misuse of Ada 95 keyword abstract in Ada 83 mode

      if Token_Name = Name_Abstract then
         Check_95_Keyword (Tok_Abstract, Tok_Tagged);
         Check_95_Keyword (Tok_Abstract, Tok_New);
      end if;

      --  Check cases of misuse of ABSTRACT

      if Token = Tok_Abstract then
         Abstract_Present := True;
         Abstract_Loc     := Token_Ptr;
         Scan; -- past ABSTRACT

         --  Ada 2005 (AI-419): AARM 3.4 (2/2)

         if (Ada_Version < Ada_2005 and then Token = Tok_Limited)
           or else Token = Tok_Private
           or else Token = Tok_Record
           or else Token = Tok_Null
         then
            Error_Msg_AP ("TAGGED expected");
         end if;
      end if;

      --  Check for misuse of Ada 95 keyword Tagged

      if Token_Name = Name_Tagged then
         Check_95_Keyword (Tok_Tagged, Tok_Private);
         Check_95_Keyword (Tok_Tagged, Tok_Limited);
         Check_95_Keyword (Tok_Tagged, Tok_Record);
      end if;

      --  Special check for misuse of Aliased

      if Token = Tok_Aliased or else Token_Name = Name_Aliased then
         Error_Msg_SC ("ALIASED not allowed in type definition");
         Scan; -- past ALIASED
      end if;

      --  The following processing deals with either a private type declaration
      --  or a full type declaration. In the private type case, we build the
      --  N_Private_Type_Declaration node, setting its Tagged_Present and
      --  Limited_Present flags, on encountering the Private keyword, and
      --  leave Typedef_Node set to Empty. For the full type declaration
      --  case, Typedef_Node gets set to the type definition.

      Typedef_Node := Empty;

      --  Switch on token following the IS. The loop normally runs once. It
      --  only runs more than once if an error is detected, to try again after
      --  detecting and fixing up the error.

      loop
         case Token is

            when Tok_Access |
                 Tok_Not    => --  Ada 2005 (AI-231)
               Typedef_Node := P_Access_Type_Definition;
               exit;

            when Tok_Array =>
               Typedef_Node := P_Array_Type_Definition;
               exit;

            when Tok_Delta =>
               Typedef_Node := P_Fixed_Point_Definition;
               exit;

            when Tok_Digits =>
               Typedef_Node := P_Floating_Point_Definition;
               exit;

            when Tok_In =>
               Ignore (Tok_In);

            when Tok_Integer_Literal =>
               T_Range;
               Typedef_Node := P_Signed_Integer_Type_Definition;
               exit;

            when Tok_Null =>
               Typedef_Node := P_Record_Definition;
               exit;

            when Tok_Left_Paren =>
               Typedef_Node := P_Enumeration_Type_Definition;

               End_Labl :=
                 Make_Identifier (Token_Ptr,
                   Chars => Chars (Ident_Node));
               Set_Comes_From_Source (End_Labl, False);

               Set_End_Label (Typedef_Node, End_Labl);
               exit;

            when Tok_Mod =>
               Typedef_Node := P_Modular_Type_Definition;
               exit;

            when Tok_New =>
               Typedef_Node := P_Derived_Type_Def_Or_Private_Ext_Decl;

               if Nkind (Typedef_Node) = N_Derived_Type_Definition
                 and then Present (Record_Extension_Part (Typedef_Node))
               then
                  End_Labl :=
                    Make_Identifier (Token_Ptr,
                      Chars => Chars (Ident_Node));
                  Set_Comes_From_Source (End_Labl, False);

                  Set_End_Label
                    (Record_Extension_Part (Typedef_Node), End_Labl);
               end if;

               exit;

            when Tok_Range =>
               Typedef_Node := P_Signed_Integer_Type_Definition;
               exit;

            when Tok_Record =>
               Typedef_Node := P_Record_Definition;

               End_Labl :=
                 Make_Identifier (Token_Ptr,
                   Chars => Chars (Ident_Node));
               Set_Comes_From_Source (End_Labl, False);

               Set_End_Label (Typedef_Node, End_Labl);
               exit;

            when Tok_Tagged =>
               Scan; -- past TAGGED

               --  Ada 2005 (AI-326): If the words IS TAGGED appear, the type
               --  is a tagged incomplete type.

               if Ada_Version >= Ada_2005
                 and then Token = Tok_Semicolon
               then
                  Scan; -- past ;

                  Decl_Node :=
                    New_Node (N_Incomplete_Type_Declaration, Type_Loc);
                  Set_Defining_Identifier           (Decl_Node, Ident_Node);
                  Set_Tagged_Present                (Decl_Node);
                  Set_Unknown_Discriminants_Present (Decl_Node, Unknown_Dis);
                  Set_Discriminant_Specifications   (Decl_Node, Discr_List);

                  return Decl_Node;
               end if;

               if Token = Tok_Abstract then
                  Error_Msg_SC -- CODEFIX
                    ("ABSTRACT must come before TAGGED");
                  Abstract_Present := True;
                  Abstract_Loc := Token_Ptr;
                  Scan; -- past ABSTRACT
               end if;

               if Token = Tok_Limited then
                  Scan; -- past LIMITED

                  --  TAGGED LIMITED PRIVATE case

                  if Token = Tok_Private then
                     Decl_Node :=
                       New_Node (N_Private_Type_Declaration, Type_Loc);
                     Set_Tagged_Present (Decl_Node, True);
                     Set_Limited_Present (Decl_Node, True);
                     Scan; -- past PRIVATE

                  --  TAGGED LIMITED RECORD

                  else
                     Typedef_Node := P_Record_Definition;
                     Set_Tagged_Present (Typedef_Node, True);
                     Set_Limited_Present (Typedef_Node, True);

                     End_Labl :=
                       Make_Identifier (Token_Ptr,
                         Chars => Chars (Ident_Node));
                     Set_Comes_From_Source (End_Labl, False);

                     Set_End_Label (Typedef_Node, End_Labl);
                  end if;

               else
                  --  TAGGED PRIVATE

                  if Token = Tok_Private then
                     Decl_Node :=
                       New_Node (N_Private_Type_Declaration, Type_Loc);
                     Set_Tagged_Present (Decl_Node, True);
                     Scan; -- past PRIVATE

                  --  TAGGED RECORD

                  else
                     Typedef_Node := P_Record_Definition;
                     Set_Tagged_Present (Typedef_Node, True);

                     End_Labl :=
                       Make_Identifier (Token_Ptr,
                         Chars => Chars (Ident_Node));
                     Set_Comes_From_Source (End_Labl, False);

                     Set_End_Label (Typedef_Node, End_Labl);
                  end if;
               end if;

               exit;

            when Tok_Limited =>
               Scan; -- past LIMITED

               loop
                  if Token = Tok_Tagged then
                     Error_Msg_SC -- CODEFIX
                       ("TAGGED must come before LIMITED");
                     Scan; -- past TAGGED

                  elsif Token = Tok_Abstract then
                     Error_Msg_SC -- CODEFIX
                       ("ABSTRACT must come before LIMITED");
                     Scan; -- past ABSTRACT

                  else
                     exit;
                  end if;
               end loop;

               --  LIMITED RECORD or LIMITED NULL RECORD

               if Token = Tok_Record or else Token = Tok_Null then
                  if Ada_Version = Ada_83 then
                     Error_Msg_SP
                       ("(Ada 83) limited record declaration not allowed!");

                  --  In Ada2005, "abstract limited" can appear before "new",
                  --  but it cannot be part of an untagged record declaration.

                  elsif Abstract_Present
                    and then Prev_Token /= Tok_Tagged
                  then
                     Error_Msg_SP ("TAGGED expected");
                  end if;

                  Typedef_Node := P_Record_Definition;
                  Set_Limited_Present (Typedef_Node, True);

               --  Ada 2005 (AI-251): LIMITED INTERFACE

               --  If we are compiling in Ada 83 or Ada 95 mode, "interface"
               --  is not a reserved word but we force its analysis to
               --  generate the corresponding usage error.

               elsif Token = Tok_Interface
                 or else (Token = Tok_Identifier
                           and then Chars (Token_Node) = Name_Interface)
               then
                  Typedef_Node :=
                    P_Interface_Type_Definition (Abstract_Present);
                  Abstract_Present := True;
                  Set_Limited_Present (Typedef_Node);

                  if Nkind (Typedef_Node) = N_Derived_Type_Definition then
                     Is_Derived_Iface := True;
                  end if;

                  --  Ada 2005 (AI-419): LIMITED NEW

               elsif Token = Tok_New then
                  if Ada_Version < Ada_2005 then
                     Error_Msg_SP
                       ("LIMITED in derived type is an Ada 2005 extension");
                     Error_Msg_SP
                       ("\unit must be compiled with -gnat05 switch");
                  end if;

                  Typedef_Node := P_Derived_Type_Def_Or_Private_Ext_Decl;
                  Set_Limited_Present (Typedef_Node);

                  if Nkind (Typedef_Node) = N_Derived_Type_Definition
                    and then Present (Record_Extension_Part (Typedef_Node))
                  then
                     End_Labl :=
                       Make_Identifier (Token_Ptr,
                                        Chars => Chars (Ident_Node));
                     Set_Comes_From_Source (End_Labl, False);

                     Set_End_Label
                       (Record_Extension_Part (Typedef_Node), End_Labl);
                  end if;

               --  LIMITED PRIVATE is the only remaining possibility here

               else
                  Decl_Node := New_Node (N_Private_Type_Declaration, Type_Loc);
                  Set_Limited_Present (Decl_Node, True);
                  T_Private; -- past PRIVATE (or complain if not there!)
               end if;

               exit;

            --  Here we have an identifier after the IS, which is certainly
            --  wrong and which might be one of several different mistakes.

            when Tok_Identifier =>

               --  First case, if identifier is on same line, then probably we
               --  have something like "type X is Integer .." and the best
               --  diagnosis is a missing NEW. Note: the missing new message
               --  will be posted by P_Derived_Type_Def_Or_Private_Ext_Decl.

               if not Token_Is_At_Start_Of_Line then
                  Typedef_Node := P_Derived_Type_Def_Or_Private_Ext_Decl;

               --  If the identifier is at the start of the line, and is in the
               --  same column as the type declaration itself then we consider
               --  that we had a missing type definition on the previous line

               elsif Start_Column <= Type_Start_Col then
                  Error_Msg_AP ("type definition expected");
                  Typedef_Node := Error;

               --  If the identifier is at the start of the line, and is in
               --  a column to the right of the type declaration line, then we
               --  may have something like:

               --    type x is
               --       r : integer

               --  and the best diagnosis is a missing record keyword

               else
                  Typedef_Node := P_Record_Definition;
               end if;

               exit;

            --  Ada 2005 (AI-251): INTERFACE

            when Tok_Interface =>
               Typedef_Node := P_Interface_Type_Definition (Abstract_Present);
               Abstract_Present := True;
               exit;

            when Tok_Private =>
               Decl_Node := New_Node (N_Private_Type_Declaration, Type_Loc);
               Scan; -- past PRIVATE

               --  Check error cases of private [abstract] tagged

               if Token = Tok_Abstract then
                  Error_Msg_SC ("`ABSTRACT TAGGED` must come before PRIVATE");
                  Scan; -- past ABSTRACT

                  if Token = Tok_Tagged then
                     Scan; -- past TAGGED
                  end if;

               elsif Token = Tok_Tagged then
                  Error_Msg_SC ("TAGGED must come before PRIVATE");
                  Scan; -- past TAGGED
               end if;

               exit;

            --  Ada 2005 (AI-345): Protected, synchronized or task interface
            --  or Ada 2005 (AI-443): Synchronized private extension.

            when Tok_Protected    |
                 Tok_Synchronized |
                 Tok_Task         =>

               declare
                  Saved_Token : constant Token_Type := Token;

               begin
                  Scan; -- past TASK, PROTECTED or SYNCHRONIZED

                  --  Synchronized private extension

                  if Token = Tok_New then
                     Typedef_Node := P_Derived_Type_Def_Or_Private_Ext_Decl;

                     if Saved_Token = Tok_Synchronized then
                        if Nkind (Typedef_Node) =
                          N_Derived_Type_Definition
                        then
                           Error_Msg_N
                             ("SYNCHRONIZED not allowed for record extension",
                              Typedef_Node);
                        else
                           Set_Synchronized_Present (Typedef_Node);
                        end if;

                     else
                        Error_Msg_SC ("invalid kind of private extension");
                     end if;

                  --  Interface

                  else
                     if Token /= Tok_Interface then
                        Error_Msg_SC ("NEW or INTERFACE expected");
                     end if;

                     Typedef_Node :=
                       P_Interface_Type_Definition (Abstract_Present);
                     Abstract_Present := True;

                     case Saved_Token is
                        when Tok_Task =>
                           Set_Task_Present         (Typedef_Node);

                        when Tok_Protected =>
                           Set_Protected_Present    (Typedef_Node);

                        when Tok_Synchronized =>
                           Set_Synchronized_Present (Typedef_Node);

                        when others =>
                           pragma Assert (False);
                           null;
                     end case;
                  end if;
               end;

               exit;

            --  Anything else is an error

            when others =>
               if Bad_Spelling_Of (Tok_Access)
                    or else
                  Bad_Spelling_Of (Tok_Array)
                    or else
                  Bad_Spelling_Of (Tok_Delta)
                    or else
                  Bad_Spelling_Of (Tok_Digits)
                    or else
                  Bad_Spelling_Of (Tok_Limited)
                    or else
                  Bad_Spelling_Of (Tok_Private)
                    or else
                  Bad_Spelling_Of (Tok_Range)
                    or else
                  Bad_Spelling_Of (Tok_Record)
                    or else
                  Bad_Spelling_Of (Tok_Tagged)
               then
                  null;

               else
                  Error_Msg_AP ("type definition expected");
                  raise Error_Resync;
               end if;

         end case;
      end loop;

      --  For the private type declaration case, the private type declaration
      --  node has been built, with the Tagged_Present and Limited_Present
      --  flags set as needed, and Typedef_Node is left set to Empty.

      if No (Typedef_Node) then
         Set_Unknown_Discriminants_Present (Decl_Node, Unknown_Dis);
         Set_Abstract_Present (Decl_Node, Abstract_Present);

      --  For a private extension declaration, Typedef_Node contains the
      --  N_Private_Extension_Declaration node, which we now complete. Note
      --  that the private extension declaration, unlike a full type
      --  declaration, does permit unknown discriminants.

      elsif Nkind (Typedef_Node) = N_Private_Extension_Declaration then
         Decl_Node := Typedef_Node;
         Set_Sloc (Decl_Node, Type_Loc);
         Set_Unknown_Discriminants_Present (Decl_Node, Unknown_Dis);
         Set_Abstract_Present (Typedef_Node, Abstract_Present);

      --  In the full type declaration case, Typedef_Node has the type
      --  definition and here is where we build the full type declaration
      --  node. This is also where we check for improper use of an unknown
      --  discriminant part (not allowed for full type declaration).

      else
         if Nkind (Typedef_Node) = N_Record_Definition
           or else (Nkind (Typedef_Node) = N_Derived_Type_Definition
                      and then Present (Record_Extension_Part (Typedef_Node)))
           or else Is_Derived_Iface
         then
            Set_Abstract_Present (Typedef_Node, Abstract_Present);

         elsif Abstract_Present then
            Error_Msg ("ABSTRACT not allowed here, ignored", Abstract_Loc);
         end if;

         Decl_Node := New_Node (N_Full_Type_Declaration, Type_Loc);
         Set_Type_Definition (Decl_Node, Typedef_Node);

         if Unknown_Dis then
            Error_Msg
              ("Full type declaration cannot have unknown discriminants",
                Discr_Sloc);
         end if;
      end if;

      --  Remaining processing is common for all three cases

      Set_Defining_Identifier (Decl_Node, Ident_Node);
      Set_Discriminant_Specifications (Decl_Node, Discr_List);
      P_Aspect_Specifications (Decl_Node);
      return Decl_Node;
   end P_Type_Declaration;

   ----------------------------------
   -- 3.2.1  Full Type Declaration --
   ----------------------------------

   --  Parsed by P_Type_Declaration (3.2.1)

   ----------------------------
   -- 3.2.1  Type Definition --
   ----------------------------

   --  Parsed by P_Type_Declaration (3.2.1)

   --------------------------------
   -- 3.2.2  Subtype Declaration --
   --------------------------------

   --  SUBTYPE_DECLARATION ::=
   --    subtype DEFINING_IDENTIFIER is [NULL_EXCLUSION] SUBTYPE_INDICATION;

   --  The caller has checked that the initial token is SUBTYPE

   --  Error recovery: can raise Error_Resync

   function P_Subtype_Declaration return Node_Id is
      Decl_Node        : Node_Id;
      Not_Null_Present : Boolean := False;

   begin
      Decl_Node := New_Node (N_Subtype_Declaration, Token_Ptr);
      Scan; -- past SUBTYPE
      Set_Defining_Identifier (Decl_Node, P_Defining_Identifier (C_Is));
      TF_Is;

      if Token = Tok_New then
         Error_Msg_SC  -- CODEFIX
           ("NEW ignored (only allowed in type declaration)");
         Scan; -- past NEW
      end if;

      Not_Null_Present := P_Null_Exclusion; --  Ada 2005 (AI-231)
      Set_Null_Exclusion_Present (Decl_Node, Not_Null_Present);

      Set_Subtype_Indication
        (Decl_Node, P_Subtype_Indication (Not_Null_Present));
      P_Aspect_Specifications (Decl_Node);
      return Decl_Node;
   end P_Subtype_Declaration;

   -------------------------------
   -- 3.2.2  Subtype Indication --
   -------------------------------

   --  SUBTYPE_INDICATION ::=
   --    [not null] SUBTYPE_MARK [CONSTRAINT]

   --  Error recovery: can raise Error_Resync

   function P_Null_Exclusion
     (Allow_Anonymous_In_95 : Boolean := False) return Boolean
   is
      Not_Loc : constant Source_Ptr := Token_Ptr;
      --  Source position of "not", if present

   begin
      if Token /= Tok_Not then
         return False;

      else
         Scan; --  past NOT

         if Token = Tok_Null then
            Scan; --  past NULL

            --  Ada 2005 (AI-441, AI-447): null_exclusion is illegal in Ada 95,
            --  except in the case of anonymous access types.

            --  Allow_Anonymous_In_95 will be True if we're parsing a formal
            --  parameter or discriminant, which are the only places where
            --  anonymous access types occur in Ada 95. "Formal : not null
            --  access ..." is legal in Ada 95, whereas "Formal : not null
            --  Named_Access_Type" is not.

            if Ada_Version >= Ada_2005
              or else (Ada_Version >= Ada_95
                        and then Allow_Anonymous_In_95
                        and then Token = Tok_Access)
            then
               null; -- OK

            else
               Error_Msg
                 ("`NOT NULL` access type is an Ada 2005 extension", Not_Loc);
               Error_Msg
                 ("\unit should be compiled with -gnat05 switch", Not_Loc);
            end if;

         else
            Error_Msg_SP ("NULL expected");
         end if;

         if Token = Tok_New then
            Error_Msg ("`NOT NULL` comes after NEW, not before", Not_Loc);
         end if;

         return True;
      end if;
   end P_Null_Exclusion;

   function P_Subtype_Indication
     (Not_Null_Present : Boolean := False) return Node_Id
   is
      Type_Node : Node_Id;

   begin
      if Token = Tok_Identifier or else Token = Tok_Operator_Symbol then
         Type_Node := P_Subtype_Mark;
         return P_Subtype_Indication (Type_Node, Not_Null_Present);

      else
         --  Check for error of using record definition and treat it nicely,
         --  otherwise things are really messed up, so resynchronize.

         if Token = Tok_Record then
            Error_Msg_SC ("anonymous record definitions are not permitted");
            Discard_Junk_Node (P_Record_Definition);
            return Error;

         else
            Error_Msg_AP ("subtype indication expected");
            raise Error_Resync;
         end if;
      end if;
   end P_Subtype_Indication;

   --  The following function is identical except that it is called with
   --  the subtype mark already scanned out, and it scans out the constraint

   --  Error recovery: can raise Error_Resync

   function P_Subtype_Indication
     (Subtype_Mark     : Node_Id;
      Not_Null_Present : Boolean := False) return Node_Id
   is
      Indic_Node  : Node_Id;
      Constr_Node : Node_Id;

   begin
      Constr_Node := P_Constraint_Opt;

      if No (Constr_Node) then
         return Subtype_Mark;
      else
         if Not_Null_Present then
            Error_Msg_SP ("`NOT NULL` not allowed if constraint given");
         end if;

         Indic_Node := New_Node (N_Subtype_Indication, Sloc (Subtype_Mark));
         Set_Subtype_Mark (Indic_Node, Check_Subtype_Mark (Subtype_Mark));
         Set_Constraint (Indic_Node, Constr_Node);
         return Indic_Node;
      end if;
   end P_Subtype_Indication;

   -------------------------
   -- 3.2.2  Subtype Mark --
   -------------------------

   --  SUBTYPE_MARK ::= subtype_NAME;

   --  Note: The subtype mark which appears after an IN or NOT IN
   --  operator is parsed by P_Range_Or_Subtype_Mark (3.5)

   --  Error recovery: cannot raise Error_Resync

   function P_Subtype_Mark return Node_Id is
   begin
      return P_Subtype_Mark_Resync;
   exception
      when Error_Resync =>
         return Error;
   end P_Subtype_Mark;

   --  This routine differs from P_Subtype_Mark in that it insists that an
   --  identifier be present, and if it is not, it raises Error_Resync.

   --  Error recovery: can raise Error_Resync

   function P_Subtype_Mark_Resync return Node_Id is
      Type_Node : Node_Id;

   begin
      if Token = Tok_Access then
         Error_Msg_SC ("anonymous access type definition not allowed here");
         Scan; -- past ACCESS
      end if;

      if Token = Tok_Array then
         Error_Msg_SC ("anonymous array definition not allowed here");
         Discard_Junk_Node (P_Array_Type_Definition);
         return Error;

      --  If Some becomes a keyword, the following is needed to make it
      --  acceptable in older versions of Ada.

      elsif Token = Tok_Some
        and then Ada_Version < Ada_2012
      then
         Scan_Reserved_Identifier (False);
         Scan;
         return Token_Node;

      else
         Type_Node := P_Qualified_Simple_Name_Resync;

         --  Check for a subtype mark attribute. The only valid possibilities
         --  are 'CLASS and 'BASE. Anything else is a definite error. We may
         --  as well catch it here.

         if Token = Tok_Apostrophe then
            return P_Subtype_Mark_Attribute (Type_Node);
         else
            return Type_Node;
         end if;
      end if;
   end P_Subtype_Mark_Resync;

   --  The following function is called to scan out a subtype mark attribute.
   --  The caller has already scanned out the subtype mark, which is passed in
   --  as the argument, and has checked that the current token is apostrophe.

   --  Only a special subclass of attributes, called type attributes
   --  (see Snames package) are allowed in this syntactic position.

   --  Note: if the apostrophe is followed by other than an identifier, then
   --  the input expression is returned unchanged, and the scan pointer is
   --  left pointing to the apostrophe.

   --  Error recovery: can raise Error_Resync

   function P_Subtype_Mark_Attribute (Type_Node : Node_Id) return Node_Id is
      Attr_Node  : Node_Id := Empty;
      Scan_State : Saved_Scan_State;
      Prefix     : Node_Id;

   begin
      Prefix := Check_Subtype_Mark (Type_Node);

      if Prefix = Error then
         raise Error_Resync;
      end if;

      --  Loop through attributes appearing (more than one can appear as for
      --  for example in X'Base'Class). We are at an apostrophe on entry to
      --  this loop, and it runs once for each attribute parsed, with
      --  Prefix being the current possible prefix if it is an attribute.

      loop
         Save_Scan_State (Scan_State); -- at Apostrophe
         Scan; -- past apostrophe

         if Token /= Tok_Identifier then
            Restore_Scan_State (Scan_State); -- to apostrophe
            return Prefix; -- no attribute after all

         elsif not Is_Type_Attribute_Name (Token_Name) then
            Error_Msg_N
              ("attribute & may not be used in a subtype mark", Token_Node);
            raise Error_Resync;

         else
            Attr_Node :=
              Make_Attribute_Reference (Prev_Token_Ptr,
                Prefix => Prefix,
                Attribute_Name => Token_Name);
            Scan; -- past type attribute identifier
         end if;

         exit when Token /= Tok_Apostrophe;
         Prefix := Attr_Node;
      end loop;

      --  Fall through here after scanning type attribute

      return Attr_Node;
   end P_Subtype_Mark_Attribute;

   -----------------------
   -- 3.2.2  Constraint --
   -----------------------

   --  CONSTRAINT ::= SCALAR_CONSTRAINT | COMPOSITE_CONSTRAINT

   --  SCALAR_CONSTRAINT ::=
   --    RANGE_CONSTRAINT | DIGITS_CONSTRAINT | DELTA_CONSTRAINT

   --  COMPOSITE_CONSTRAINT ::=
   --    INDEX_CONSTRAINT | DISCRIMINANT_CONSTRAINT

   --  If no constraint is present, this function returns Empty

   --  Error recovery: can raise Error_Resync

   function P_Constraint_Opt return Node_Id is
   begin
      if Token = Tok_Range
        or else Bad_Spelling_Of (Tok_Range)
      then
         return P_Range_Constraint;

      elsif Token = Tok_Digits
        or else Bad_Spelling_Of (Tok_Digits)
      then
         return P_Digits_Constraint;

      elsif Token = Tok_Delta
        or else Bad_Spelling_Of (Tok_Delta)
      then
         return P_Delta_Constraint;

      elsif Token = Tok_Left_Paren then
         return P_Index_Or_Discriminant_Constraint;

      elsif Token = Tok_In then
         Ignore (Tok_In);
         return P_Constraint_Opt;

      else
         return Empty;
      end if;
   end P_Constraint_Opt;

   ------------------------------
   -- 3.2.2  Scalar Constraint --
   ------------------------------

   --  Parsed by P_Constraint_Opt (3.2.2)

   ---------------------------------
   -- 3.2.2  Composite Constraint --
   ---------------------------------

   --  Parsed by P_Constraint_Opt (3.2.2)

   --------------------------------------------------------
   -- 3.3  Identifier Declarations (Also 7.4, 8.5, 11.1) --
   --------------------------------------------------------

   --  This routine scans out a declaration starting with an identifier:

   --  OBJECT_DECLARATION ::=
   --    DEFINING_IDENTIFIER_LIST : [aliased] [constant]
   --      [NULL_EXCLUSION] SUBTYPE_INDICATION [:= EXPRESSION]
   --        [ASPECT_SPECIFICATIONS];
   --  | DEFINING_IDENTIFIER_LIST : [aliased] [constant]
   --      ACCESS_DEFINITION [:= EXPRESSION]
   --        [ASPECT_SPECIFICATIONS];
   --  | DEFINING_IDENTIFIER_LIST : [aliased] [constant]
   --      ARRAY_TYPE_DEFINITION [:= EXPRESSION]
   --        [ASPECT_SPECIFICATIONS];

   --  NUMBER_DECLARATION ::=
   --    DEFINING_IDENTIFIER_LIST : constant ::= static_EXPRESSION;

   --  OBJECT_RENAMING_DECLARATION ::=
   --    DEFINING_IDENTIFIER :
   --      [NULL_EXCLUSION] SUBTYPE_MARK renames object_NAME;
   --  | DEFINING_IDENTIFIER :
   --      ACCESS_DEFINITION renames object_NAME;

   --  EXCEPTION_RENAMING_DECLARATION ::=
   --    DEFINING_IDENTIFIER : exception renames exception_NAME;

   --  EXCEPTION_DECLARATION ::=
   --    DEFINING_IDENTIFIER_LIST : exception
   --      [ASPECT_SPECIFICATIONS];

   --  Note that the ALIASED indication in an object declaration is
   --  marked by a flag in the parent node.

   --  The caller has checked that the initial token is an identifier

   --  The value returned is a list of declarations, one for each identifier
   --  in the list (as described in Sinfo, we always split up multiple
   --  declarations into the equivalent sequence of single declarations
   --  using the More_Ids and Prev_Ids flags to preserve the source).

   --  If the identifier turns out to be a probable statement rather than
   --  an identifier, then the scan is left pointing to the identifier and
   --  No_List is returned.

   --  Error recovery: can raise Error_Resync

   procedure P_Identifier_Declarations
     (Decls   : List_Id;
      Done    : out Boolean;
      In_Spec : Boolean)
   is
      Acc_Node         : Node_Id;
      Decl_Node        : Node_Id;
      Type_Node        : Node_Id;
      Ident_Sloc       : Source_Ptr;
      Scan_State       : Saved_Scan_State;
      List_OK          : Boolean := True;
      Ident            : Nat;
      Init_Expr        : Node_Id;
      Init_Loc         : Source_Ptr;
      Con_Loc          : Source_Ptr;
      Not_Null_Present : Boolean := False;

      Idents : array (Int range 1 .. 4096) of Entity_Id;
      --  Used to save identifiers in the identifier list. The upper bound
      --  of 4096 is expected to be infinite in practice, and we do not even
      --  bother to check if this upper bound is exceeded.

      Num_Idents : Nat := 1;
      --  Number of identifiers stored in Idents

      procedure No_List;
      --  This procedure is called in renames cases to make sure that we do
      --  not have more than one identifier. If we do have more than one
      --  then an error message is issued (and the declaration is split into
      --  multiple declarations)

      function Token_Is_Renames return Boolean;
      --  Checks if current token is RENAMES, and if so, scans past it and
      --  returns True, otherwise returns False. Includes checking for some
      --  common error cases.

      -------------
      -- No_List --
      -------------

      procedure No_List is
      begin
         if Num_Idents > 1 then
            Error_Msg
              ("identifier list not allowed for RENAMES",
               Sloc (Idents (2)));
         end if;

         List_OK := False;
      end No_List;

      ----------------------
      -- Token_Is_Renames --
      ----------------------

      function Token_Is_Renames return Boolean is
         At_Colon : Saved_Scan_State;

      begin
         if Token = Tok_Colon then
            Save_Scan_State (At_Colon);
            Scan; -- past colon
            Check_Misspelling_Of (Tok_Renames);

            if Token = Tok_Renames then
               Error_Msg_SP -- CODEFIX
                 ("|extra "":"" ignored");
               Scan; -- past RENAMES
               return True;
            else
               Restore_Scan_State (At_Colon);
               return False;
            end if;

         else
            Check_Misspelling_Of (Tok_Renames);

            if Token = Tok_Renames then
               Scan; -- past RENAMES
               return True;
            else
               return False;
            end if;
         end if;
      end Token_Is_Renames;

   --  Start of processing for P_Identifier_Declarations

   begin
      Ident_Sloc := Token_Ptr;
      Save_Scan_State (Scan_State); -- at first identifier
      Idents (1) := P_Defining_Identifier (C_Comma_Colon);

      --  If we have a colon after the identifier, then we can assume that
      --  this is in fact a valid identifier declaration and can steam ahead.

      if Token = Tok_Colon then
         Scan; -- past colon

      --  If we have a comma, then scan out the list of identifiers

      elsif Token = Tok_Comma then
         while Comma_Present loop
            Num_Idents := Num_Idents + 1;
            Idents (Num_Idents) := P_Defining_Identifier (C_Comma_Colon);
         end loop;

         Save_Scan_State (Scan_State); -- at colon
         T_Colon;

      --  If we have identifier followed by := then we assume that what is
      --  really meant is an assignment statement. The assignment statement
      --  is scanned out and added to the list of declarations. An exception
      --  occurs if the := is followed by the keyword constant, in which case
      --  we assume it was meant to be a colon.

      elsif Token = Tok_Colon_Equal then
         Scan; -- past :=

         if Token = Tok_Constant then
            Error_Msg_SP ("colon expected");

         else
            Restore_Scan_State (Scan_State);
            Statement_When_Declaration_Expected (Decls, Done, In_Spec);
            return;
         end if;

      --  If we have an IS keyword, then assume the TYPE keyword was missing

      elsif Token = Tok_Is then
         Restore_Scan_State (Scan_State);
         Append_To (Decls, P_Type_Declaration);
         Done := False;
         return;

      --  Otherwise we have an error situation

      else
         Restore_Scan_State (Scan_State);

         --  First case is possible misuse of PROTECTED in Ada 83 mode. If
         --  so, fix the keyword and return to scan the protected declaration.

         if Token_Name = Name_Protected then
            Check_95_Keyword (Tok_Protected, Tok_Identifier);
            Check_95_Keyword (Tok_Protected, Tok_Type);
            Check_95_Keyword (Tok_Protected, Tok_Body);

            if Token = Tok_Protected then
               Done := False;
               return;
            end if;

         --  Check misspelling possibilities. If so, correct the misspelling
         --  and return to scan out the resulting declaration.

         elsif Bad_Spelling_Of (Tok_Function)
           or else Bad_Spelling_Of (Tok_Procedure)
           or else Bad_Spelling_Of (Tok_Package)
           or else Bad_Spelling_Of (Tok_Pragma)
           or else Bad_Spelling_Of (Tok_Protected)
           or else Bad_Spelling_Of (Tok_Generic)
           or else Bad_Spelling_Of (Tok_Subtype)
           or else Bad_Spelling_Of (Tok_Type)
           or else Bad_Spelling_Of (Tok_Task)
           or else Bad_Spelling_Of (Tok_Use)
           or else Bad_Spelling_Of (Tok_For)
         then
            Done := False;
            return;

         --  Otherwise we definitely have an ordinary identifier with a junk
         --  token after it. Just complain that we expect a declaration, and
         --  skip to a semicolon

         else
            Set_Declaration_Expected;
            Resync_Past_Semicolon;
            Done := False;
            return;
         end if;
      end if;

      --  Come here with an identifier list and colon scanned out. We now
      --  build the nodes for the declarative items. One node is built for
      --  each identifier in the list, with the type information being
      --  repeated by rescanning the appropriate section of source.

      --  First an error check, if we have two identifiers in a row, a likely
      --  possibility is that the first of the identifiers is an incorrectly
      --  spelled keyword.

      if Token = Tok_Identifier then
         declare
            SS : Saved_Scan_State;
            I2 : Boolean;

         begin
            Save_Scan_State (SS);
            Scan; -- past initial identifier
            I2 := (Token = Tok_Identifier);
            Restore_Scan_State (SS);

            if I2
              and then
                (Bad_Spelling_Of (Tok_Access)   or else
                 Bad_Spelling_Of (Tok_Aliased)  or else
                 Bad_Spelling_Of (Tok_Constant))
            then
               null;
            end if;
         end;
      end if;

      --  Loop through identifiers

      Ident := 1;
      Ident_Loop : loop

         --  Check for some cases of misused Ada 95 keywords

         if Token_Name = Name_Aliased then
            Check_95_Keyword (Tok_Aliased, Tok_Array);
            Check_95_Keyword (Tok_Aliased, Tok_Identifier);
            Check_95_Keyword (Tok_Aliased, Tok_Constant);
         end if;

         --  Constant cases

         if Token = Tok_Constant then
            Con_Loc := Token_Ptr;
            Scan; -- past CONSTANT

            --  Number declaration, initialization required

            Init_Expr := Init_Expr_Opt;

            if Present (Init_Expr) then
               if Not_Null_Present then
                  Error_Msg_SP
                    ("`NOT NULL` not allowed in numeric expression");
               end if;

               Decl_Node := New_Node (N_Number_Declaration, Ident_Sloc);
               Set_Expression (Decl_Node, Init_Expr);

            --  Constant object declaration

            else
               Decl_Node := New_Node (N_Object_Declaration, Ident_Sloc);
               Set_Constant_Present (Decl_Node, True);

               if Token_Name = Name_Aliased then
                  Check_95_Keyword (Tok_Aliased, Tok_Array);
                  Check_95_Keyword (Tok_Aliased, Tok_Identifier);
               end if;

               if Token = Tok_Aliased then
                  Error_Msg_SC -- CODEFIX
                    ("ALIASED should be before CONSTANT");
                  Scan; -- past ALIASED
                  Set_Aliased_Present (Decl_Node, True);
               end if;

               if Token = Tok_Array then
                  Set_Object_Definition
                    (Decl_Node, P_Array_Type_Definition);

               else
                  Not_Null_Present := P_Null_Exclusion; --  Ada 2005 (AI-231)
                  Set_Null_Exclusion_Present (Decl_Node, Not_Null_Present);

                  if Token = Tok_Access then
                     if Ada_Version < Ada_2005 then
                        Error_Msg_SP
                          ("generalized use of anonymous access types " &
                           "is an Ada 2005 extension");
                        Error_Msg_SP
                          ("\unit must be compiled with -gnat05 switch");
                     end if;

                     Set_Object_Definition
                       (Decl_Node, P_Access_Definition (Not_Null_Present));
                  else
                     Set_Object_Definition
                       (Decl_Node, P_Subtype_Indication (Not_Null_Present));
                  end if;
               end if;

               if Token = Tok_Renames then
                  Error_Msg
                    ("CONSTANT not permitted in renaming declaration",
                     Con_Loc);
                  Scan; -- Past renames
                  Discard_Junk_Node (P_Name);
               end if;
            end if;

         --  Exception cases

         elsif Token = Tok_Exception then
            Scan; -- past EXCEPTION

            if Token_Is_Renames then
               No_List;
               Decl_Node :=
                 New_Node (N_Exception_Renaming_Declaration, Ident_Sloc);
               Set_Name (Decl_Node, P_Qualified_Simple_Name_Resync);
               No_Constraint;
            else
               Decl_Node := New_Node (N_Exception_Declaration, Prev_Token_Ptr);
            end if;

         --  Aliased case (note that an object definition is required)

         elsif Token = Tok_Aliased then
            Scan; -- past ALIASED
            Decl_Node := New_Node (N_Object_Declaration, Ident_Sloc);
            Set_Aliased_Present (Decl_Node, True);

            if Token = Tok_Constant then
               Scan; -- past CONSTANT
               Set_Constant_Present (Decl_Node, True);
            end if;

            if Token = Tok_Array then
               Set_Object_Definition
                 (Decl_Node, P_Array_Type_Definition);

            else
               Not_Null_Present := P_Null_Exclusion; --  Ada 2005 (AI-231)
               Set_Null_Exclusion_Present (Decl_Node, Not_Null_Present);

               --  Access definition (AI-406) or subtype indication

               if Token = Tok_Access then
                  if Ada_Version < Ada_2005 then
                     Error_Msg_SP
                       ("generalized use of anonymous access types " &
                        "is an Ada 2005 extension");
                     Error_Msg_SP
                       ("\unit must be compiled with -gnat05 switch");
                  end if;

                  Set_Object_Definition
                    (Decl_Node, P_Access_Definition (Not_Null_Present));
               else
                  Set_Object_Definition
                    (Decl_Node, P_Subtype_Indication (Not_Null_Present));
               end if;
            end if;

         --  Array case

         elsif Token = Tok_Array then
            Decl_Node := New_Node (N_Object_Declaration, Ident_Sloc);
            Set_Object_Definition (Decl_Node, P_Array_Type_Definition);

         --  Ada 2005 (AI-254, AI-406)

         elsif Token = Tok_Not then

            --  OBJECT_DECLARATION ::=
            --    DEFINING_IDENTIFIER_LIST : [aliased] [constant]
            --      [NULL_EXCLUSION] SUBTYPE_INDICATION [:= EXPRESSION];
            --  | DEFINING_IDENTIFIER_LIST : [aliased] [constant]
            --      ACCESS_DEFINITION [:= EXPRESSION];

            --  OBJECT_RENAMING_DECLARATION ::=
            --    DEFINING_IDENTIFIER :
            --      [NULL_EXCLUSION] SUBTYPE_MARK renames object_NAME;
            --  | DEFINING_IDENTIFIER :
            --      ACCESS_DEFINITION renames object_NAME;

            Not_Null_Present := P_Null_Exclusion;  --  Ada 2005 (AI-231/423)

            if Token = Tok_Access then
               if Ada_Version < Ada_2005 then
                  Error_Msg_SP
                    ("generalized use of anonymous access types " &
                     "is an Ada 2005 extension");
                  Error_Msg_SP ("\unit must be compiled with -gnat05 switch");
               end if;

               Acc_Node := P_Access_Definition (Not_Null_Present);

               if Token /= Tok_Renames then
                  Decl_Node := New_Node (N_Object_Declaration, Ident_Sloc);
                  Set_Object_Definition (Decl_Node, Acc_Node);

               else
                  Scan; --  past renames
                  No_List;
                  Decl_Node :=
                    New_Node (N_Object_Renaming_Declaration, Ident_Sloc);
                  Set_Access_Definition (Decl_Node, Acc_Node);
                  Set_Name (Decl_Node, P_Name);
               end if;

            else
               Type_Node := P_Subtype_Mark;

               --  Object renaming declaration

               if Token_Is_Renames then
                  if Ada_Version < Ada_2005 then
                     Error_Msg_SP
                       ("`NOT NULL` not allowed in object renaming");
                     raise Error_Resync;

                  --  Ada 2005 (AI-423): Object renaming declaration with
                  --  a null exclusion.

                  else
                     No_List;
                     Decl_Node :=
                       New_Node (N_Object_Renaming_Declaration, Ident_Sloc);
                     Set_Null_Exclusion_Present (Decl_Node, Not_Null_Present);
                     Set_Subtype_Mark (Decl_Node, Type_Node);
                     Set_Name (Decl_Node, P_Name);
                  end if;

               --  Object declaration

               else
                  Decl_Node := New_Node (N_Object_Declaration, Ident_Sloc);
                  Set_Null_Exclusion_Present (Decl_Node, Not_Null_Present);
                  Set_Object_Definition
                    (Decl_Node,
                     P_Subtype_Indication (Type_Node, Not_Null_Present));

                  --  RENAMES at this point means that we had the combination
                  --  of a constraint on the Type_Node and renames, which is
                  --  illegal

                  if Token_Is_Renames then
                     Error_Msg_N
                       ("constraint not allowed in object renaming "
                        & "declaration",
                        Constraint (Object_Definition (Decl_Node)));
                     raise Error_Resync;
                  end if;
               end if;
            end if;

         --  Ada 2005 (AI-230): Access Definition case

         elsif Token = Tok_Access then
            if Ada_Version < Ada_2005 then
               Error_Msg_SP
                 ("generalized use of anonymous access types " &
                  "is an Ada 2005 extension");
               Error_Msg_SP ("\unit must be compiled with -gnat05 switch");
            end if;

            Acc_Node := P_Access_Definition (Null_Exclusion_Present => False);

            --  Object declaration with access definition, or renaming

            if Token /= Tok_Renames then
               Decl_Node := New_Node (N_Object_Declaration, Ident_Sloc);
               Set_Object_Definition (Decl_Node, Acc_Node);

            else
               Scan; --  past renames
               No_List;
               Decl_Node :=
                 New_Node (N_Object_Renaming_Declaration, Ident_Sloc);
               Set_Access_Definition (Decl_Node, Acc_Node);
               Set_Name (Decl_Node, P_Name);
            end if;

         --  Subtype indication case

         else
            Type_Node := P_Subtype_Mark;

            --  Object renaming declaration

            if Token_Is_Renames then
               No_List;
               Decl_Node :=
                 New_Node (N_Object_Renaming_Declaration, Ident_Sloc);
               Set_Subtype_Mark (Decl_Node, Type_Node);
               Set_Name (Decl_Node, P_Name);

            --  Object declaration

            else
               Decl_Node := New_Node (N_Object_Declaration, Ident_Sloc);
               Set_Null_Exclusion_Present (Decl_Node, Not_Null_Present);
               Set_Object_Definition
                 (Decl_Node,
                  P_Subtype_Indication (Type_Node, Not_Null_Present));

               --  RENAMES at this point means that we had the combination of
               --  a constraint on the Type_Node and renames, which is illegal

               if Token_Is_Renames then
                  Error_Msg_N
                    ("constraint not allowed in object renaming declaration",
                     Constraint (Object_Definition (Decl_Node)));
                  raise Error_Resync;
               end if;
            end if;
         end if;

         --  Scan out initialization, allowed only for object declaration

         Init_Loc := Token_Ptr;
         Init_Expr := Init_Expr_Opt;

         if Present (Init_Expr) then
            if Nkind (Decl_Node) = N_Object_Declaration then
               Set_Expression (Decl_Node, Init_Expr);
               Set_Has_Init_Expression (Decl_Node);
            else
               Error_Msg ("initialization not allowed here", Init_Loc);
            end if;
         end if;

         Set_Defining_Identifier (Decl_Node, Idents (Ident));
         P_Aspect_Specifications (Decl_Node);

         if List_OK then
            if Ident < Num_Idents then
               Set_More_Ids (Decl_Node, True);
            end if;

            if Ident > 1 then
               Set_Prev_Ids (Decl_Node, True);
            end if;
         end if;

         Append (Decl_Node, Decls);
         exit Ident_Loop when Ident = Num_Idents;
         Restore_Scan_State (Scan_State);
         T_Colon;
         Ident := Ident + 1;
      end loop Ident_Loop;

      Done := False;
   end P_Identifier_Declarations;

   -------------------------------
   -- 3.3.1  Object Declaration --
   -------------------------------

   --  OBJECT DECLARATION ::=
   --    DEFINING_IDENTIFIER_LIST : [aliased] [constant]
   --      SUBTYPE_INDICATION [:= EXPRESSION];
   --  | DEFINING_IDENTIFIER_LIST : [aliased] [constant]
   --      ARRAY_TYPE_DEFINITION [:= EXPRESSION];
   --  | SINGLE_TASK_DECLARATION
   --  | SINGLE_PROTECTED_DECLARATION

   --  Cases starting with TASK are parsed by P_Task (9.1)
   --  Cases starting with PROTECTED are parsed by P_Protected (9.4)
   --  All other cases are parsed by P_Identifier_Declarations (3.3)

   -------------------------------------
   -- 3.3.1  Defining Identifier List --
   -------------------------------------

   --  DEFINING_IDENTIFIER_LIST ::=
   --    DEFINING_IDENTIFIER {, DEFINING_IDENTIFIER}

   --  Always parsed by the construct in which it appears. See special
   --  section on "Handling of Defining Identifier Lists" in this unit.

   -------------------------------
   -- 3.3.2  Number Declaration --
   -------------------------------

   --  Parsed by P_Identifier_Declarations (3.3)

   -------------------------------------------------------------------------
   -- 3.4  Derived Type Definition or Private Extension Declaration (7.3) --
   -------------------------------------------------------------------------

   --  DERIVED_TYPE_DEFINITION ::=
   --    [abstract] [limited] new [NULL_EXCLUSION] parent_SUBTYPE_INDICATION
   --    [[and INTERFACE_LIST] RECORD_EXTENSION_PART]

   --  PRIVATE_EXTENSION_DECLARATION ::=
   --     type DEFINING_IDENTIFIER [DISCRIMINANT_PART] is
   --       [abstract] [limited | synchronized]
   --          new ancestor_SUBTYPE_INDICATION [and INTERFACE_LIST]
   --            with private;

   --  RECORD_EXTENSION_PART ::= with RECORD_DEFINITION

   --  The caller has already scanned out the part up to the NEW, and Token
   --  either contains Tok_New (or ought to, if it doesn't this procedure
   --  will post an appropriate "NEW expected" message).

   --  Note: the caller is responsible for filling in the Sloc field of
   --  the returned node in the private extension declaration case as
   --  well as the stuff relating to the discriminant part.

   --  Error recovery: can raise Error_Resync;

   function P_Derived_Type_Def_Or_Private_Ext_Decl return Node_Id is
      Typedef_Node     : Node_Id;
      Typedecl_Node    : Node_Id;
      Not_Null_Present : Boolean := False;

   begin
      Typedef_Node := New_Node (N_Derived_Type_Definition, Token_Ptr);

      if Ada_Version < Ada_2005
        and then Token = Tok_Identifier
        and then Token_Name = Name_Interface
      then
         Error_Msg_SP
           ("abstract interface is an Ada 2005 extension");
         Error_Msg_SP ("\unit must be compiled with -gnat05 switch");
      else
         T_New;
      end if;

      if Token = Tok_Abstract then
         Error_Msg_SC -- CODEFIX
           ("ABSTRACT must come before NEW, not after");
         Scan;
      end if;

      Not_Null_Present := P_Null_Exclusion; --  Ada 2005 (AI-231)
      Set_Null_Exclusion_Present (Typedef_Node, Not_Null_Present);
      Set_Subtype_Indication (Typedef_Node,
         P_Subtype_Indication (Not_Null_Present));

      --  Ada 2005 (AI-251): Deal with interfaces

      if Token = Tok_And then
         Scan; -- past AND

         if Ada_Version < Ada_2005 then
            Error_Msg_SP
              ("abstract interface is an Ada 2005 extension");
            Error_Msg_SP ("\unit must be compiled with -gnat05 switch");
         end if;

         Set_Interface_List (Typedef_Node, New_List);

         loop
            Append (P_Qualified_Simple_Name, Interface_List (Typedef_Node));
            exit when Token /= Tok_And;
            Scan; -- past AND
         end loop;

         if Token /= Tok_With then
            Error_Msg_SC ("WITH expected");
            raise Error_Resync;
         end if;
      end if;

      --  Deal with record extension, note that we assume that a WITH is
      --  missing in the case of "type X is new Y record ..." or in the
      --  case of "type X is new Y null record".

      --  First make sure we don't have an aspect specification. If we do
      --  return now, so that our caller can check it (the WITH here is not
      --  part of a type extension).

      if Aspect_Specifications_Present then
         return Typedef_Node;

      --  OK, not an aspect specification, so continue test for extension

      elsif Token = Tok_With
        or else Token = Tok_Record
        or else Token = Tok_Null
      then
         T_With; -- past WITH or give error message

         if Token = Tok_Limited then
            Error_Msg_SC ("LIMITED keyword not allowed in private extension");
            Scan; -- ignore LIMITED
         end if;

         --  Private extension declaration

         if Token = Tok_Private then
            Scan; -- past PRIVATE

            --  Throw away the type definition node and build the type
            --  declaration node. Note the caller must set the Sloc,
            --  Discriminant_Specifications, Unknown_Discriminants_Present,
            --  and Defined_Identifier fields in the returned node.

            Typedecl_Node :=
              Make_Private_Extension_Declaration (No_Location,
                Defining_Identifier => Empty,
                Subtype_Indication  => Subtype_Indication (Typedef_Node),
                Abstract_Present    => Abstract_Present (Typedef_Node),
                Interface_List      => Interface_List (Typedef_Node));

            return Typedecl_Node;

         --  Derived type definition with record extension part

         else
            Set_Record_Extension_Part (Typedef_Node, P_Record_Definition);
            return Typedef_Node;
         end if;

      --  Derived type definition with no record extension part

      else
         return Typedef_Node;
      end if;
   end P_Derived_Type_Def_Or_Private_Ext_Decl;

   ---------------------------
   -- 3.5  Range Constraint --
   ---------------------------

   --  RANGE_CONSTRAINT ::= range RANGE

   --  The caller has checked that the initial token is RANGE

   --  Error recovery: cannot raise Error_Resync

   function P_Range_Constraint return Node_Id is
      Range_Node : Node_Id;

   begin
      Range_Node := New_Node (N_Range_Constraint, Token_Ptr);
      Scan; -- past RANGE
      Set_Range_Expression (Range_Node, P_Range);
      return Range_Node;
   end P_Range_Constraint;

   ----------------
   -- 3.5  Range --
   ----------------

   --  RANGE ::=
   --    RANGE_ATTRIBUTE_REFERENCE | SIMPLE_EXPRESSION .. SIMPLE_EXPRESSION

   --  Note: the range that appears in a membership test is parsed by
   --  P_Range_Or_Subtype_Mark (3.5).

   --  Error recovery: cannot raise Error_Resync

   function P_Range return Node_Id is
      Expr_Node  : Node_Id;
      Range_Node : Node_Id;

   begin
      Expr_Node := P_Simple_Expression_Or_Range_Attribute;

      if Expr_Form = EF_Range_Attr then
         return Expr_Node;

      elsif Token = Tok_Dot_Dot then
         Range_Node := New_Node (N_Range, Token_Ptr);
         Set_Low_Bound (Range_Node, Expr_Node);
         Scan; -- past ..
         Expr_Node := P_Expression;
         Check_Simple_Expression (Expr_Node);
         Set_High_Bound (Range_Node, Expr_Node);
         return Range_Node;

      --  Anything else is an error

      else
         T_Dot_Dot; -- force missing .. message
         return Error;
      end if;
   end P_Range;

   ----------------------------------
   -- 3.5  P_Range_Or_Subtype_Mark --
   ----------------------------------

   --  RANGE ::=
   --    RANGE_ATTRIBUTE_REFERENCE
   --  | SIMPLE_EXPRESSION .. SIMPLE_EXPRESSION

   --  This routine scans out the range or subtype mark that forms the right
   --  operand of a membership test (it is not used in any other contexts, and
   --  error messages are specialized with this knowledge in mind).

   --  Note: as documented in the Sinfo interface, although the syntax only
   --  allows a subtype mark, we in fact allow any simple expression to be
   --  returned from this routine. The semantics is responsible for issuing
   --  an appropriate message complaining if the argument is not a name.
   --  This simplifies the coding and error recovery processing in the
   --  parser, and in any case it is preferable not to consider this a
   --  syntax error and to continue with the semantic analysis.

   --  Error recovery: cannot raise Error_Resync

   function P_Range_Or_Subtype_Mark
     (Allow_Simple_Expression : Boolean := False) return Node_Id
   is
      Expr_Node  : Node_Id;
      Range_Node : Node_Id;
      Save_Loc   : Source_Ptr;

   --  Start of processing for P_Range_Or_Subtype_Mark

   begin
      --  Save location of possible junk parentheses

      Save_Loc := Token_Ptr;

      --  Scan out either a simple expression or a range (this accepts more
      --  than is legal here, but as explained above, we like to allow more
      --  with a proper diagnostic, and in the case of a membership operation
      --  where sets are allowed, a simple expression is permissible anyway.

      Expr_Node := P_Simple_Expression_Or_Range_Attribute;

      --  Range attribute

      if Expr_Form = EF_Range_Attr then
         return Expr_Node;

      --  Simple_Expression .. Simple_Expression

      elsif Token = Tok_Dot_Dot then
         Check_Simple_Expression (Expr_Node);
         Range_Node := New_Node (N_Range, Token_Ptr);
         Set_Low_Bound (Range_Node, Expr_Node);
         Scan; -- past ..
         Set_High_Bound (Range_Node, P_Simple_Expression);
         return Range_Node;

      --  Case of subtype mark (optionally qualified simple name or an
      --  attribute whose prefix is an optionally qualified simple name)

      elsif Expr_Form = EF_Simple_Name
        or else Nkind (Expr_Node) = N_Attribute_Reference
      then
         --  Check for error of range constraint after a subtype mark

         if Token = Tok_Range then
            Error_Msg_SC ("range constraint not allowed in membership test");
            Scan; -- past RANGE
            raise Error_Resync;

         --  Check for error of DIGITS or DELTA after a subtype mark

         elsif Token = Tok_Digits or else Token = Tok_Delta then
            Error_Msg_SC
              ("accuracy definition not allowed in membership test");
            Scan; -- past DIGITS or DELTA
            raise Error_Resync;

         --  Attribute reference, may or may not be OK, but in any case we
         --  will scan it out

         elsif Token = Tok_Apostrophe then
            return P_Subtype_Mark_Attribute (Expr_Node);

         --  OK case of simple name, just return it

         else
            return Expr_Node;
         end if;

      --  Simple expression case

      elsif Expr_Form = EF_Simple and then Allow_Simple_Expression then
         return Expr_Node;

      --  Here we have some kind of error situation. Check for junk parens
      --  then return what we have, caller will deal with other errors.

      else
         if Nkind (Expr_Node) in N_Subexpr
           and then Paren_Count (Expr_Node) /= 0
         then
            Error_Msg ("|parentheses not allowed for subtype mark", Save_Loc);
            Set_Paren_Count (Expr_Node, 0);
         end if;

         return Expr_Node;
      end if;
   end P_Range_Or_Subtype_Mark;

   ----------------------------------------
   -- 3.5.1  Enumeration Type Definition --
   ----------------------------------------

   --  ENUMERATION_TYPE_DEFINITION ::=
   --    (ENUMERATION_LITERAL_SPECIFICATION
   --      {, ENUMERATION_LITERAL_SPECIFICATION})

   --  The caller has already scanned out the TYPE keyword

   --  Error recovery: can raise Error_Resync;

   function P_Enumeration_Type_Definition return Node_Id is
      Typedef_Node : Node_Id;

   begin
      Typedef_Node := New_Node (N_Enumeration_Type_Definition, Token_Ptr);
      Set_Literals (Typedef_Node, New_List);

      T_Left_Paren;

      loop
         Append (P_Enumeration_Literal_Specification, Literals (Typedef_Node));
         exit when not Comma_Present;
      end loop;

      T_Right_Paren;
      return Typedef_Node;
   end P_Enumeration_Type_Definition;

   ----------------------------------------------
   -- 3.5.1  Enumeration Literal Specification --
   ----------------------------------------------

   --  ENUMERATION_LITERAL_SPECIFICATION ::=
   --    DEFINING_IDENTIFIER | DEFINING_CHARACTER_LITERAL

   --  Error recovery: can raise Error_Resync

   function P_Enumeration_Literal_Specification return Node_Id is
   begin
      if Token = Tok_Char_Literal then
         return P_Defining_Character_Literal;
      else
         return P_Defining_Identifier (C_Comma_Right_Paren);
      end if;
   end P_Enumeration_Literal_Specification;

   ---------------------------------------
   -- 3.5.1  Defining_Character_Literal --
   ---------------------------------------

   --  DEFINING_CHARACTER_LITERAL ::= CHARACTER_LITERAL

   --  Error recovery: cannot raise Error_Resync

   --  The caller has checked that the current token is a character literal

   function P_Defining_Character_Literal return Node_Id is
      Literal_Node : Node_Id;
   begin
      Literal_Node := Token_Node;
      Change_Character_Literal_To_Defining_Character_Literal (Literal_Node);
      Scan; -- past character literal
      return Literal_Node;
   end P_Defining_Character_Literal;

   ------------------------------------
   -- 3.5.4  Integer Type Definition --
   ------------------------------------

   --  Parsed by P_Type_Declaration (3.2.1)

   -------------------------------------------
   -- 3.5.4  Signed Integer Type Definition --
   -------------------------------------------

   --  SIGNED_INTEGER_TYPE_DEFINITION ::=
   --    range static_SIMPLE_EXPRESSION .. static_SIMPLE_EXPRESSION

   --  Normally the initial token on entry is RANGE, but in some
   --  error conditions, the range token was missing and control is
   --  passed with Token pointing to first token of the first expression.

   --  Error recovery: cannot raise Error_Resync

   function P_Signed_Integer_Type_Definition return Node_Id is
      Typedef_Node : Node_Id;
      Expr_Node    : Node_Id;

   begin
      Typedef_Node := New_Node (N_Signed_Integer_Type_Definition, Token_Ptr);

      if Token = Tok_Range then
         Scan; -- past RANGE
      end if;

      Expr_Node := P_Expression;
      Check_Simple_Expression (Expr_Node);
      Set_Low_Bound (Typedef_Node, Expr_Node);
      T_Dot_Dot;
      Expr_Node := P_Expression;
      Check_Simple_Expression (Expr_Node);
      Set_High_Bound (Typedef_Node, Expr_Node);
      return Typedef_Node;
   end P_Signed_Integer_Type_Definition;

   ------------------------------------
   -- 3.5.4  Modular Type Definition --
   ------------------------------------

   --  MODULAR_TYPE_DEFINITION ::= mod static_EXPRESSION

   --  The caller has checked that the initial token is MOD

   --  Error recovery: cannot raise Error_Resync

   function P_Modular_Type_Definition return Node_Id is
      Typedef_Node : Node_Id;

   begin
      if Ada_Version = Ada_83 then
         Error_Msg_SC ("(Ada 83): modular types not allowed");
      end if;

      Typedef_Node := New_Node (N_Modular_Type_Definition, Token_Ptr);
      Scan; -- past MOD
      Set_Expression (Typedef_Node, P_Expression_No_Right_Paren);

      --  Handle mod L..R cleanly

      if Token = Tok_Dot_Dot then
         Error_Msg_SC ("range not allowed for modular type");
         Scan; -- past ..
         Set_Expression (Typedef_Node, P_Expression_No_Right_Paren);
      end if;

      return Typedef_Node;
   end P_Modular_Type_Definition;

   ---------------------------------
   -- 3.5.6  Real Type Definition --
   ---------------------------------

   --  Parsed by P_Type_Declaration (3.2.1)

   --------------------------------------
   -- 3.5.7  Floating Point Definition --
   --------------------------------------

   --  FLOATING_POINT_DEFINITION ::=
   --    digits static_EXPRESSION [REAL_RANGE_SPECIFICATION]

   --  Note: In Ada-83, the EXPRESSION must be a SIMPLE_EXPRESSION

   --  The caller has checked that the initial token is DIGITS

   --  Error recovery: cannot raise Error_Resync

   function P_Floating_Point_Definition return Node_Id is
      Digits_Loc : constant Source_Ptr := Token_Ptr;
      Def_Node   : Node_Id;
      Expr_Node  : Node_Id;

   begin
      Scan; -- past DIGITS
      Expr_Node := P_Expression_No_Right_Paren;
      Check_Simple_Expression_In_Ada_83 (Expr_Node);

      --  Handle decimal fixed-point defn with DIGITS/DELTA in wrong order

      if Token = Tok_Delta then
         Error_Msg_SC -- CODEFIX
           ("|DELTA must come before DIGITS");
         Def_Node := New_Node (N_Decimal_Fixed_Point_Definition, Digits_Loc);
         Scan; -- past DELTA
         Set_Delta_Expression (Def_Node, P_Expression_No_Right_Paren);

      --  OK floating-point definition

      else
         Def_Node := New_Node (N_Floating_Point_Definition, Digits_Loc);
      end if;

      Set_Digits_Expression (Def_Node, Expr_Node);
      Set_Real_Range_Specification (Def_Node, P_Real_Range_Specification_Opt);
      return Def_Node;
   end P_Floating_Point_Definition;

   -------------------------------------
   -- 3.5.7  Real Range Specification --
   -------------------------------------

   --  REAL_RANGE_SPECIFICATION ::=
   --    range static_SIMPLE_EXPRESSION .. static_SIMPLE_EXPRESSION

   --  Error recovery: cannot raise Error_Resync

   function P_Real_Range_Specification_Opt return Node_Id is
      Specification_Node : Node_Id;
      Expr_Node          : Node_Id;

   begin
      if Token = Tok_Range then
         Specification_Node :=
           New_Node (N_Real_Range_Specification, Token_Ptr);
         Scan; -- past RANGE
         Expr_Node := P_Expression_No_Right_Paren;
         Check_Simple_Expression (Expr_Node);
         Set_Low_Bound (Specification_Node, Expr_Node);
         T_Dot_Dot;
         Expr_Node := P_Expression_No_Right_Paren;
         Check_Simple_Expression (Expr_Node);
         Set_High_Bound (Specification_Node, Expr_Node);
         return Specification_Node;
      else
         return Empty;
      end if;
   end P_Real_Range_Specification_Opt;

   -----------------------------------
   -- 3.5.9  Fixed Point Definition --
   -----------------------------------

   --  FIXED_POINT_DEFINITION ::=
   --    ORDINARY_FIXED_POINT_DEFINITION | DECIMAL_FIXED_POINT_DEFINITION

   --  ORDINARY_FIXED_POINT_DEFINITION ::=
   --    delta static_EXPRESSION REAL_RANGE_SPECIFICATION

   --  DECIMAL_FIXED_POINT_DEFINITION ::=
   --    delta static_EXPRESSION
   --      digits static_EXPRESSION [REAL_RANGE_SPECIFICATION]

   --  The caller has checked that the initial token is DELTA

   --  Error recovery: cannot raise Error_Resync

   function P_Fixed_Point_Definition return Node_Id is
      Delta_Node : Node_Id;
      Delta_Loc  : Source_Ptr;
      Def_Node   : Node_Id;
      Expr_Node  : Node_Id;

   begin
      Delta_Loc := Token_Ptr;
      Scan; -- past DELTA
      Delta_Node := P_Expression_No_Right_Paren;
      Check_Simple_Expression_In_Ada_83 (Delta_Node);

      if Token = Tok_Digits then
         if Ada_Version = Ada_83 then
            Error_Msg_SC ("(Ada 83) decimal fixed type not allowed!");
         end if;

         Def_Node := New_Node (N_Decimal_Fixed_Point_Definition, Delta_Loc);
         Scan; -- past DIGITS
         Expr_Node := P_Expression_No_Right_Paren;
         Check_Simple_Expression_In_Ada_83 (Expr_Node);
         Set_Digits_Expression (Def_Node, Expr_Node);

      else
         Def_Node := New_Node (N_Ordinary_Fixed_Point_Definition, Delta_Loc);

         --  Range is required in ordinary fixed point case

         if Token /= Tok_Range then
            Error_Msg_AP ("range must be given for fixed-point type");
            T_Range;
         end if;
      end if;

      Set_Delta_Expression (Def_Node, Delta_Node);
      Set_Real_Range_Specification (Def_Node, P_Real_Range_Specification_Opt);
      return Def_Node;
   end P_Fixed_Point_Definition;

   --------------------------------------------
   -- 3.5.9  Ordinary Fixed Point Definition --
   --------------------------------------------

   --  Parsed by P_Fixed_Point_Definition (3.5.9)

   -------------------------------------------
   -- 3.5.9  Decimal Fixed Point Definition --
   -------------------------------------------

   --  Parsed by P_Decimal_Point_Definition (3.5.9)

   ------------------------------
   -- 3.5.9  Digits Constraint --
   ------------------------------

   --  DIGITS_CONSTRAINT ::=
   --    digits static_EXPRESSION [RANGE_CONSTRAINT]

   --  Note: in Ada 83, the EXPRESSION must be a SIMPLE_EXPRESSION

   --  The caller has checked that the initial token is DIGITS

   function P_Digits_Constraint return Node_Id is
      Constraint_Node : Node_Id;
      Expr_Node : Node_Id;

   begin
      Constraint_Node := New_Node (N_Digits_Constraint, Token_Ptr);
      Scan; -- past DIGITS
      Expr_Node := P_Expression;
      Check_Simple_Expression_In_Ada_83 (Expr_Node);
      Set_Digits_Expression (Constraint_Node, Expr_Node);

      if Token = Tok_Range then
         Set_Range_Constraint (Constraint_Node, P_Range_Constraint);
      end if;

      return Constraint_Node;
   end P_Digits_Constraint;

   -----------------------------
   -- 3.5.9  Delta Constraint --
   -----------------------------

   --  DELTA CONSTRAINT ::= DELTA STATIC_EXPRESSION [RANGE_CONSTRAINT]

   --  Note: this is an obsolescent feature in Ada 95 (I.3)

   --  Note: in Ada 83, the EXPRESSION must be a SIMPLE_EXPRESSION

   --  The caller has checked that the initial token is DELTA

   --  Error recovery: cannot raise Error_Resync

   function P_Delta_Constraint return Node_Id is
      Constraint_Node : Node_Id;
      Expr_Node : Node_Id;

   begin
      Constraint_Node := New_Node (N_Delta_Constraint, Token_Ptr);
      Scan; -- past DELTA
      Expr_Node := P_Expression;
      Check_Simple_Expression_In_Ada_83 (Expr_Node);
      Set_Delta_Expression (Constraint_Node, Expr_Node);

      if Token = Tok_Range then
         Set_Range_Constraint (Constraint_Node, P_Range_Constraint);
      end if;

      return Constraint_Node;
   end P_Delta_Constraint;

   --------------------------------
   -- 3.6  Array Type Definition --
   --------------------------------

   --  ARRAY_TYPE_DEFINITION ::=
   --    UNCONSTRAINED_ARRAY_DEFINITION | CONSTRAINED_ARRAY_DEFINITION

   --  UNCONSTRAINED_ARRAY_DEFINITION ::=
   --    array (INDEX_SUBTYPE_DEFINITION {, INDEX_SUBTYPE_DEFINITION}) of
   --      COMPONENT_DEFINITION

   --  INDEX_SUBTYPE_DEFINITION ::= SUBTYPE_MARK range <>

   --  CONSTRAINED_ARRAY_DEFINITION ::=
   --    array (DISCRETE_SUBTYPE_DEFINITION {, DISCRETE_SUBTYPE_DEFINITION}) of
   --      COMPONENT_DEFINITION

   --  DISCRETE_SUBTYPE_DEFINITION ::=
   --    DISCRETE_SUBTYPE_INDICATION | RANGE

   --  COMPONENT_DEFINITION ::=
   --    [aliased] [NULL_EXCLUSION] SUBTYPE_INDICATION | ACCESS_DEFINITION

   --  The caller has checked that the initial token is ARRAY

   --  Error recovery: can raise Error_Resync

   function P_Array_Type_Definition return Node_Id is
      Array_Loc        : Source_Ptr;
      CompDef_Node     : Node_Id;
      Def_Node         : Node_Id;
      Not_Null_Present : Boolean := False;
      Subs_List        : List_Id;
      Scan_State       : Saved_Scan_State;
      Aliased_Present  : Boolean := False;

   begin
      Array_Loc := Token_Ptr;
      Scan; -- past ARRAY
      Subs_List := New_List;
      T_Left_Paren;

      --  It's quite tricky to disentangle these two possibilities, so we do
      --  a prescan to determine which case we have and then reset the scan.
      --  The prescan skips past possible subtype mark tokens.

      Save_Scan_State (Scan_State); -- just after paren

      while Token in Token_Class_Desig or else
            Token = Tok_Dot or else
            Token = Tok_Apostrophe -- because of 'BASE, 'CLASS
      loop
         Scan;
      end loop;

      --  If we end up on RANGE <> then we have the unconstrained case. We
      --  will also allow the RANGE to be omitted, just to improve error
      --  handling for a case like array (integer <>) of integer;

      Scan; -- past possible RANGE or <>

      if (Prev_Token = Tok_Range and then Token = Tok_Box) or else
         Prev_Token = Tok_Box
      then
         Def_Node := New_Node (N_Unconstrained_Array_Definition, Array_Loc);
         Restore_Scan_State (Scan_State); -- to first subtype mark

         loop
            Append (P_Subtype_Mark_Resync, Subs_List);
            T_Range;
            T_Box;
            exit when Token = Tok_Right_Paren or else Token = Tok_Of;
            T_Comma;
         end loop;

         Set_Subtype_Marks (Def_Node, Subs_List);

      else
         Def_Node := New_Node (N_Constrained_Array_Definition, Array_Loc);
         Restore_Scan_State (Scan_State); -- to first discrete range

         loop
            Append (P_Discrete_Subtype_Definition, Subs_List);
            exit when not Comma_Present;
         end loop;

         Set_Discrete_Subtype_Definitions (Def_Node, Subs_List);
      end if;

      T_Right_Paren;
      T_Of;

      CompDef_Node := New_Node (N_Component_Definition, Token_Ptr);

      if Token_Name = Name_Aliased then
         Check_95_Keyword (Tok_Aliased, Tok_Identifier);
      end if;

      if Token = Tok_Aliased then
         Aliased_Present := True;
         Scan; -- past ALIASED
      end if;

      Not_Null_Present := P_Null_Exclusion; --  Ada 2005 (AI-231/AI-254)

      --  Ada 2005 (AI-230): Access Definition case

      if Token = Tok_Access then
         if Ada_Version < Ada_2005 then
            Error_Msg_SP
              ("generalized use of anonymous access types " &
               "is an Ada 2005 extension");
            Error_Msg_SP ("\unit must be compiled with -gnat05 switch");
         end if;

         if Aliased_Present then
            Error_Msg_SP ("ALIASED not allowed here");
         end if;

         Set_Subtype_Indication     (CompDef_Node, Empty);
         Set_Aliased_Present        (CompDef_Node, False);
         Set_Access_Definition      (CompDef_Node,
           P_Access_Definition (Not_Null_Present));
      else

         Set_Access_Definition      (CompDef_Node, Empty);
         Set_Aliased_Present        (CompDef_Node, Aliased_Present);
         Set_Null_Exclusion_Present (CompDef_Node, Not_Null_Present);
         Set_Subtype_Indication     (CompDef_Node,
           P_Subtype_Indication (Not_Null_Present));
      end if;

      Set_Component_Definition (Def_Node, CompDef_Node);

      return Def_Node;
   end P_Array_Type_Definition;

   -----------------------------------------
   -- 3.6  Unconstrained Array Definition --
   -----------------------------------------

   --  Parsed by P_Array_Type_Definition (3.6)

   ---------------------------------------
   -- 3.6  Constrained Array Definition --
   ---------------------------------------

   --  Parsed by P_Array_Type_Definition (3.6)

   --------------------------------------
   -- 3.6  Discrete Subtype Definition --
   --------------------------------------

   --  DISCRETE_SUBTYPE_DEFINITION ::=
   --    discrete_SUBTYPE_INDICATION | RANGE

   --  Note: the discrete subtype definition appearing in a constrained
   --  array definition is parsed by P_Array_Type_Definition (3.6)

   --  Error recovery: cannot raise Error_Resync

   function P_Discrete_Subtype_Definition return Node_Id is
   begin
      --  The syntax of a discrete subtype definition is identical to that
      --  of a discrete range, so we simply share the same parsing code.

      return P_Discrete_Range;
   end P_Discrete_Subtype_Definition;

   -------------------------------
   -- 3.6  Component Definition --
   -------------------------------

   --  For the array case, parsed by P_Array_Type_Definition (3.6)
   --  For the record case, parsed by P_Component_Declaration (3.8)

   -----------------------------
   -- 3.6.1  Index Constraint --
   -----------------------------

   --  Parsed by P_Index_Or_Discriminant_Constraint (3.7.1)

   ---------------------------
   -- 3.6.1  Discrete Range --
   ---------------------------

   --  DISCRETE_RANGE ::= discrete_SUBTYPE_INDICATION | RANGE

   --  The possible forms for a discrete range are:

      --   Subtype_Mark                           (SUBTYPE_INDICATION, 3.2.2)
      --   Subtype_Mark range Range               (SUBTYPE_INDICATION, 3.2.2)
      --   Range_Attribute                        (RANGE, 3.5)
      --   Simple_Expression .. Simple_Expression (RANGE, 3.5)

   --  Error recovery: cannot raise Error_Resync

   function P_Discrete_Range return Node_Id is
      Expr_Node  : Node_Id;
      Range_Node : Node_Id;

   begin
      Expr_Node := P_Simple_Expression_Or_Range_Attribute;

      if Expr_Form = EF_Range_Attr then
         return Expr_Node;

      elsif Token = Tok_Range then
         if Expr_Form /= EF_Simple_Name then
            Error_Msg_SC ("range must be preceded by subtype mark");
         end if;

         return P_Subtype_Indication (Expr_Node);

      --  Check Expression .. Expression case

      elsif Token = Tok_Dot_Dot then
         Range_Node := New_Node (N_Range, Token_Ptr);
         Set_Low_Bound (Range_Node, Expr_Node);
         Scan; -- past ..
         Expr_Node := P_Expression;
         Check_Simple_Expression (Expr_Node);
         Set_High_Bound (Range_Node, Expr_Node);
         return Range_Node;

      --  Otherwise we must have a subtype mark

      elsif Expr_Form = EF_Simple_Name then
         return Expr_Node;

      --  If incorrect, complain that we expect ..

      else
         T_Dot_Dot;
         return Expr_Node;
      end if;
   end P_Discrete_Range;

   ----------------------------
   -- 3.7  Discriminant Part --
   ----------------------------

   --  DISCRIMINANT_PART ::=
   --    UNKNOWN_DISCRIMINANT_PART
   --  | KNOWN_DISCRIMINANT_PART

   --  A discriminant part is parsed by P_Known_Discriminant_Part_Opt (3.7)
   --  or P_Unknown_Discriminant_Part (3.7), since we know which we want.

   ------------------------------------
   -- 3.7  Unknown Discriminant Part --
   ------------------------------------

   --  UNKNOWN_DISCRIMINANT_PART ::= (<>)

   --  If no unknown discriminant part is present, then False is returned,
   --  otherwise the unknown discriminant is scanned out and True is returned.

   --  Error recovery: cannot raise Error_Resync

   function P_Unknown_Discriminant_Part_Opt return Boolean is
      Scan_State : Saved_Scan_State;

   begin
      --  If <> right now, then this is missing left paren

      if Token = Tok_Box then
         U_Left_Paren;

      --  If not <> or left paren, then definitely no box

      elsif Token /= Tok_Left_Paren then
         return False;

      --  Left paren, so might be a box after it

      else
         Save_Scan_State (Scan_State);
         Scan; -- past the left paren

         if Token /= Tok_Box then
            Restore_Scan_State (Scan_State);
            return False;
         end if;
      end if;

      --  We are now pointing to the box

      if Ada_Version = Ada_83 then
         Error_Msg_SC ("(Ada 83) unknown discriminant not allowed!");
      end if;

      Scan; -- past the box
      U_Right_Paren; -- must be followed by right paren
      return True;
   end P_Unknown_Discriminant_Part_Opt;

   ----------------------------------
   -- 3.7  Known Discriminant Part --
   ----------------------------------

   --  KNOWN_DISCRIMINANT_PART ::=
   --    (DISCRIMINANT_SPECIFICATION {; DISCRIMINANT_SPECIFICATION})

   --  DISCRIMINANT_SPECIFICATION ::=
   --    DEFINING_IDENTIFIER_LIST : [NULL_EXCLUSION] SUBTYPE_MARK
   --      [:= DEFAULT_EXPRESSION]
   --  | DEFINING_IDENTIFIER_LIST : ACCESS_DEFINITION
   --      [:= DEFAULT_EXPRESSION]

   --  If no known discriminant part is present, then No_List is returned

   --  Error recovery: cannot raise Error_Resync

   function P_Known_Discriminant_Part_Opt return List_Id is
      Specification_Node : Node_Id;
      Specification_List : List_Id;
      Ident_Sloc         : Source_Ptr;
      Scan_State         : Saved_Scan_State;
      Num_Idents         : Nat;
      Not_Null_Present   : Boolean;
      Ident              : Nat;

      Idents : array (Int range 1 .. 4096) of Entity_Id;
      --  This array holds the list of defining identifiers. The upper bound
      --  of 4096 is intended to be essentially infinite, and we do not even
      --  bother to check for it being exceeded.

   begin
      if Token = Tok_Left_Paren then
         Specification_List := New_List;
         Scan; -- past (
         P_Pragmas_Misplaced;

         Specification_Loop : loop

            Ident_Sloc := Token_Ptr;
            Idents (1) := P_Defining_Identifier (C_Comma_Colon);
            Num_Idents := 1;

            while Comma_Present loop
               Num_Idents := Num_Idents + 1;
               Idents (Num_Idents) := P_Defining_Identifier (C_Comma_Colon);
            end loop;

            --  If there are multiple identifiers, we repeatedly scan the
            --  type and initialization expression information by resetting
            --  the scan pointer (so that we get completely separate trees
            --  for each occurrence).

            if Num_Idents > 1 then
               Save_Scan_State (Scan_State);
            end if;

            T_Colon;

            --  Loop through defining identifiers in list

            Ident := 1;
            Ident_Loop : loop
               Specification_Node :=
                 New_Node (N_Discriminant_Specification, Ident_Sloc);
               Set_Defining_Identifier (Specification_Node, Idents (Ident));
               Not_Null_Present :=  --  Ada 2005 (AI-231, AI-447)
                 P_Null_Exclusion (Allow_Anonymous_In_95 => True);

               if Token = Tok_Access then
                  if Ada_Version = Ada_83 then
                     Error_Msg_SC
                       ("(Ada 83) access discriminant not allowed!");
                  end if;

                  Set_Discriminant_Type
                    (Specification_Node,
                     P_Access_Definition (Not_Null_Present));
               else

                  Set_Discriminant_Type
                    (Specification_Node, P_Subtype_Mark);
                  No_Constraint;
                  Set_Null_Exclusion_Present  -- Ada 2005 (AI-231)
                    (Specification_Node, Not_Null_Present);
               end if;

               Set_Expression
                 (Specification_Node, Init_Expr_Opt (True));

               if Ident > 1 then
                  Set_Prev_Ids (Specification_Node, True);
               end if;

               if Ident < Num_Idents then
                  Set_More_Ids (Specification_Node, True);
               end if;

               Append (Specification_Node, Specification_List);
               exit Ident_Loop when Ident = Num_Idents;
               Ident := Ident + 1;
               Restore_Scan_State (Scan_State);
               T_Colon;
            end loop Ident_Loop;

            exit Specification_Loop when Token /= Tok_Semicolon;
            Scan; -- past ;
            P_Pragmas_Misplaced;
         end loop Specification_Loop;

         T_Right_Paren;
         return Specification_List;

      else
         return No_List;
      end if;
   end P_Known_Discriminant_Part_Opt;

   -------------------------------------
   -- 3.7  Discriminant Specification --
   -------------------------------------

   --  Parsed by P_Known_Discriminant_Part_Opt (3.7)

   -----------------------------
   -- 3.7  Default Expression --
   -----------------------------

   --  Always parsed (simply as an Expression) by the parent construct

   ------------------------------------
   -- 3.7.1  Discriminant Constraint --
   ------------------------------------

   --  Parsed by P_Index_Or_Discriminant_Constraint (3.7.1)

   --------------------------------------------------------
   -- 3.7.1  Index or Discriminant Constraint (also 3.6) --
   --------------------------------------------------------

   --  DISCRIMINANT_CONSTRAINT ::=
   --    (DISCRIMINANT_ASSOCIATION {, DISCRIMINANT_ASSOCIATION})

   --  DISCRIMINANT_ASSOCIATION ::=
   --    [discriminant_SELECTOR_NAME {| discriminant_SELECTOR_NAME} =>]
   --      EXPRESSION

   --  This routine parses either an index or a discriminant constraint. As
   --  is clear from the above grammar, it is often possible to clearly
   --  determine which of the two possibilities we have, but there are
   --  cases (those in which we have a series of expressions of the same
   --  syntactic form as subtype indications), where we cannot tell. Since
   --  this means that in any case the semantic phase has to distinguish
   --  between the two, there is not much point in the parser trying to
   --  distinguish even those cases where the difference is clear. In any
   --  case, if we have a situation like:

   --     (A => 123, 235 .. 500)

   --  it is not clear which of the two items is the wrong one, better to
   --  let the semantic phase give a clear message. Consequently, this
   --  routine in general returns a list of items which can be either
   --  discrete ranges or discriminant associations.

   --  The caller has checked that the initial token is a left paren

   --  Error recovery: can raise Error_Resync

   function P_Index_Or_Discriminant_Constraint return Node_Id is
      Scan_State  : Saved_Scan_State;
      Constr_Node : Node_Id;
      Constr_List : List_Id;
      Expr_Node   : Node_Id;
      Result_Node : Node_Id;

   begin
      Result_Node := New_Node (N_Index_Or_Discriminant_Constraint, Token_Ptr);
      Scan; -- past (
      Constr_List := New_List;
      Set_Constraints (Result_Node, Constr_List);

      --  The two syntactic forms are a little mixed up, so what we are doing
      --  here is looking at the first entry to determine which case we have

      --  A discriminant constraint is a list of discriminant associations,
      --  which have one of the following possible forms:

      --    Expression
      --    Id => Expression
      --    Id | Id | .. | Id => Expression

      --  An index constraint is a list of discrete ranges which have one
      --  of the following possible forms:

      --    Subtype_Mark
      --    Subtype_Mark range Range
      --    Range_Attribute
      --    Simple_Expression .. Simple_Expression

      --  Loop through discriminants in list

      loop
         --  Check cases of Id => Expression or Id | Id => Expression

         if Token = Tok_Identifier then
            Save_Scan_State (Scan_State); -- at Id
            Scan; -- past Id

            if Token = Tok_Arrow or else Token = Tok_Vertical_Bar then
               Restore_Scan_State (Scan_State); -- to Id
               Append (P_Discriminant_Association, Constr_List);
               goto Loop_Continue;
            else
               Restore_Scan_State (Scan_State); -- to Id
            end if;
         end if;

         --  Otherwise scan out an expression and see what we have got

         Expr_Node := P_Expression_Or_Range_Attribute;

         if Expr_Form = EF_Range_Attr then
            Append (Expr_Node, Constr_List);

         elsif Token = Tok_Range then
            if Expr_Form /= EF_Simple_Name then
               Error_Msg_SC ("subtype mark required before RANGE");
            end if;

            Append (P_Subtype_Indication (Expr_Node), Constr_List);
            goto Loop_Continue;

         --  Check Simple_Expression .. Simple_Expression case

         elsif Token = Tok_Dot_Dot then
            Check_Simple_Expression (Expr_Node);
            Constr_Node := New_Node (N_Range, Token_Ptr);
            Set_Low_Bound (Constr_Node, Expr_Node);
            Scan; -- past ..
            Expr_Node := P_Expression;
            Check_Simple_Expression (Expr_Node);
            Set_High_Bound (Constr_Node, Expr_Node);
            Append (Constr_Node, Constr_List);
            goto Loop_Continue;

         --  Case of an expression which could be either form

         else
            Append (Expr_Node, Constr_List);
            goto Loop_Continue;
         end if;

         --  Here with a single entry scanned

         <<Loop_Continue>>
            exit when not Comma_Present;

      end loop;

      T_Right_Paren;
      return Result_Node;
   end P_Index_Or_Discriminant_Constraint;

   -------------------------------------
   -- 3.7.1  Discriminant Association --
   -------------------------------------

   --  DISCRIMINANT_ASSOCIATION ::=
   --    [discriminant_SELECTOR_NAME {| discriminant_SELECTOR_NAME} =>]
   --      EXPRESSION

   --  This routine is used only when the name list is present and the caller
   --  has already checked this (by scanning ahead and repositioning the
   --  scan).

   --  Error_Recovery: cannot raise Error_Resync;

   function P_Discriminant_Association return Node_Id is
      Discr_Node : Node_Id;
      Names_List : List_Id;
      Ident_Sloc : Source_Ptr;

   begin
      Ident_Sloc := Token_Ptr;
      Names_List := New_List;

      loop
         Append (P_Identifier (C_Vertical_Bar_Arrow), Names_List);
         exit when Token /= Tok_Vertical_Bar;
         Scan; -- past |
      end loop;

      Discr_Node := New_Node (N_Discriminant_Association, Ident_Sloc);
      Set_Selector_Names (Discr_Node, Names_List);
      TF_Arrow;
      Set_Expression (Discr_Node, P_Expression);
      return Discr_Node;
   end P_Discriminant_Association;

   ---------------------------------
   -- 3.8  Record Type Definition --
   ---------------------------------

   --  RECORD_TYPE_DEFINITION ::=
   --    [[abstract] tagged] [limited] RECORD_DEFINITION

   --  There is no node in the tree for a record type definition. Instead
   --  a record definition node appears, with possible Abstract_Present,
   --  Tagged_Present, and Limited_Present flags set appropriately.

   ----------------------------
   -- 3.8  Record Definition --
   ----------------------------

   --  RECORD_DEFINITION ::=
   --    record
   --      COMPONENT_LIST
   --    end record
   --  | null record

   --  Note: in the case where a record definition node is used to represent
   --  a record type definition, the caller sets the Tagged_Present and
   --  Limited_Present flags in the resulting N_Record_Definition node as
   --  required.

   --  Note that the RECORD token at the start may be missing in certain
   --  error situations, so this function is expected to post the error

   --  Error recovery: can raise Error_Resync

   function P_Record_Definition return Node_Id is
      Rec_Node : Node_Id;

   begin
      Rec_Node := New_Node (N_Record_Definition, Token_Ptr);

      --  Null record case

      if Token = Tok_Null then
         Scan; -- past NULL
         T_Record;
         Set_Null_Present (Rec_Node, True);

      --  Catch incomplete declaration to prevent cascaded errors, see
      --  ACATS B393002 for an example.

      elsif Token = Tok_Semicolon then
         Error_Msg_AP ("missing record definition");

      --  Case starting with RECORD keyword. Build scope stack entry. For the
      --  column, we use the first non-blank character on the line, to deal
      --  with situations such as:

      --    type X is record
      --      ...
      --    end record;

      --  which is not official RM indentation, but is not uncommon usage, and
      --  in particular is standard GNAT coding style, so handle it nicely.

      else
         Push_Scope_Stack;
         Scope.Table (Scope.Last).Etyp := E_Record;
         Scope.Table (Scope.Last).Ecol := Start_Column;
         Scope.Table (Scope.Last).Sloc := Token_Ptr;
         Scope.Table (Scope.Last).Labl := Error;
         Scope.Table (Scope.Last).Junk := (Token /= Tok_Record);

         T_Record;

         Set_Component_List (Rec_Node, P_Component_List);

         loop
            exit when Check_End;
            Discard_Junk_Node (P_Component_List);
         end loop;
      end if;

      return Rec_Node;
   end P_Record_Definition;

   -------------------------
   -- 3.8  Component List --
   -------------------------

   --  COMPONENT_LIST ::=
   --    COMPONENT_ITEM {COMPONENT_ITEM}
   --  | {COMPONENT_ITEM} VARIANT_PART
   --  | null;

   --  Error recovery: cannot raise Error_Resync

   function P_Component_List return Node_Id is
      Component_List_Node : Node_Id;
      Decls_List          : List_Id;
      Scan_State          : Saved_Scan_State;

   begin
      Component_List_Node := New_Node (N_Component_List, Token_Ptr);
      Decls_List := New_List;

      if Token = Tok_Null then
         Scan; -- past NULL
         TF_Semicolon;
         P_Pragmas_Opt (Decls_List);
         Set_Null_Present (Component_List_Node, True);
         return Component_List_Node;

      else
         P_Pragmas_Opt (Decls_List);

         if Token /= Tok_Case then
            Component_Scan_Loop : loop
               P_Component_Items (Decls_List);
               P_Pragmas_Opt (Decls_List);

               exit Component_Scan_Loop when Token = Tok_End
                 or else Token = Tok_Case
                 or else Token = Tok_When;

               --  We are done if we do not have an identifier. However, if
               --  we have a misspelled reserved identifier that is in a column
               --  to the right of the record definition, we will treat it as
               --  an identifier. It turns out to be too dangerous in practice
               --  to accept such a mis-spelled identifier which does not have
               --  this additional clue that confirms the incorrect spelling.

               if Token /= Tok_Identifier then
                  if Start_Column > Scope.Table (Scope.Last).Ecol
                    and then Is_Reserved_Identifier
                  then
                     Save_Scan_State (Scan_State); -- at reserved id
                     Scan; -- possible reserved id

                     if Token = Tok_Comma or else Token = Tok_Colon then
                        Restore_Scan_State (Scan_State);
                        Scan_Reserved_Identifier (Force_Msg => True);

                     --  Note reserved identifier used as field name after
                     --  all because not followed by colon or comma

                     else
                        Restore_Scan_State (Scan_State);
                        exit Component_Scan_Loop;
                     end if;

                  --  Non-identifier that definitely was not reserved id

                  else
                     exit Component_Scan_Loop;
                  end if;
               end if;
            end loop Component_Scan_Loop;
         end if;

         if Token = Tok_Case then
            Set_Variant_Part (Component_List_Node, P_Variant_Part);

            --  Check for junk after variant part

            if Token = Tok_Identifier then
               Save_Scan_State (Scan_State);
               Scan; -- past identifier

               if Token = Tok_Colon then
                  Restore_Scan_State (Scan_State);
                  Error_Msg_SC ("component may not follow variant part");
                  Discard_Junk_Node (P_Component_List);

               elsif Token = Tok_Case then
                  Restore_Scan_State (Scan_State);
                  Error_Msg_SC ("only one variant part allowed in a record");
                  Discard_Junk_Node (P_Component_List);

               else
                  Restore_Scan_State (Scan_State);
               end if;
            end if;
         end if;
      end if;

      Set_Component_Items (Component_List_Node, Decls_List);
      return Component_List_Node;
   end P_Component_List;

   -------------------------
   -- 3.8  Component Item --
   -------------------------

   --  COMPONENT_ITEM ::= COMPONENT_DECLARATION | REPRESENTATION_CLAUSE

   --  COMPONENT_DECLARATION ::=
   --    DEFINING_IDENTIFIER_LIST : COMPONENT_DEFINITION
   --      [:= DEFAULT_EXPRESSION]
   --        [ASPECT_SPECIFICATIONS];

   --  COMPONENT_DEFINITION ::=
   --    [aliased] [NULL_EXCLUSION] SUBTYPE_INDICATION | ACCESS_DEFINITION

   --  Error recovery: cannot raise Error_Resync, if an error occurs,
   --  the scan is positioned past the following semicolon.

   --  Note: we do not yet allow representation clauses to appear as component
   --  items, do we need to add this capability sometime in the future ???

   procedure P_Component_Items (Decls : List_Id) is
      Aliased_Present  : Boolean := False;
      CompDef_Node     : Node_Id;
      Decl_Node        : Node_Id;
      Scan_State       : Saved_Scan_State;
      Not_Null_Present : Boolean := False;
      Num_Idents       : Nat;
      Ident            : Nat;
      Ident_Sloc       : Source_Ptr;

      Idents : array (Int range 1 .. 4096) of Entity_Id;
      --  This array holds the list of defining identifiers. The upper bound
      --  of 4096 is intended to be essentially infinite, and we do not even
      --  bother to check for it being exceeded.

   begin
      if Token /= Tok_Identifier then
         Error_Msg_SC ("component declaration expected");
         Resync_Past_Semicolon;
         return;
      end if;

      Ident_Sloc := Token_Ptr;
      Idents (1) := P_Defining_Identifier (C_Comma_Colon);
      Num_Idents := 1;

      while Comma_Present loop
         Num_Idents := Num_Idents + 1;
         Idents (Num_Idents) := P_Defining_Identifier (C_Comma_Colon);
      end loop;

      --  If there are multiple identifiers, we repeatedly scan the
      --  type and initialization expression information by resetting
      --  the scan pointer (so that we get completely separate trees
      --  for each occurrence).

      if Num_Idents > 1 then
         Save_Scan_State (Scan_State);
      end if;

      T_Colon;

      --  Loop through defining identifiers in list

      Ident := 1;
      Ident_Loop : loop

         --  The following block is present to catch Error_Resync
         --  which causes the parse to be reset past the semicolon

         begin
            Decl_Node := New_Node (N_Component_Declaration, Ident_Sloc);
            Set_Defining_Identifier (Decl_Node, Idents (Ident));

            if Token = Tok_Constant then
               Error_Msg_SC ("constant components are not permitted");
               Scan;
            end if;

            CompDef_Node := New_Node (N_Component_Definition, Token_Ptr);

            if Token_Name = Name_Aliased then
               Check_95_Keyword (Tok_Aliased, Tok_Identifier);
            end if;

            if Token = Tok_Aliased then
               Aliased_Present := True;
               Scan; -- past ALIASED
            end if;

            Not_Null_Present := P_Null_Exclusion; -- Ada 2005 (AI-231/AI-254)

            --  Ada 2005 (AI-230): Access Definition case

            if Token = Tok_Access then
               if Ada_Version < Ada_2005 then
                  Error_Msg_SP
                    ("generalized use of anonymous access types " &
                     "is an Ada 2005 extension");
                  Error_Msg_SP ("\unit must be compiled with -gnat05 switch");
               end if;

               if Aliased_Present then
                  Error_Msg_SP ("ALIASED not allowed here");
               end if;

               Set_Subtype_Indication (CompDef_Node, Empty);
               Set_Aliased_Present    (CompDef_Node, False);
               Set_Access_Definition  (CompDef_Node,
                 P_Access_Definition (Not_Null_Present));
            else

               Set_Access_Definition      (CompDef_Node, Empty);
               Set_Aliased_Present        (CompDef_Node, Aliased_Present);
               Set_Null_Exclusion_Present (CompDef_Node, Not_Null_Present);

               if Token = Tok_Array then
                  Error_Msg_SC ("anonymous arrays not allowed as components");
                  raise Error_Resync;
               end if;

               Set_Subtype_Indication (CompDef_Node,
                 P_Subtype_Indication (Not_Null_Present));
            end if;

            Set_Component_Definition (Decl_Node, CompDef_Node);
            Set_Expression           (Decl_Node, Init_Expr_Opt);

            if Ident > 1 then
               Set_Prev_Ids (Decl_Node, True);
            end if;

            if Ident < Num_Idents then
               Set_More_Ids (Decl_Node, True);
            end if;

            Append (Decl_Node, Decls);

         exception
            when Error_Resync =>
               if Token /= Tok_End then
                  Resync_Past_Semicolon;
               end if;
         end;

         exit Ident_Loop when Ident = Num_Idents;
         Ident := Ident + 1;
         Restore_Scan_State (Scan_State);
         T_Colon;
      end loop Ident_Loop;

      P_Aspect_Specifications (Decl_Node);
   end P_Component_Items;

   --------------------------------
   -- 3.8  Component Declaration --
   --------------------------------

   --  Parsed by P_Component_Items (3.8)

   -------------------------
   -- 3.8.1  Variant Part --
   -------------------------

   --  VARIANT_PART ::=
   --    case discriminant_DIRECT_NAME is
   --      VARIANT
   --      {VARIANT}
   --    end case;

   --  The caller has checked that the initial token is CASE

   --  Error recovery: cannot raise Error_Resync

   function P_Variant_Part return Node_Id is
      Variant_Part_Node : Node_Id;
      Variants_List     : List_Id;
      Case_Node         : Node_Id;

   begin
      Variant_Part_Node := New_Node (N_Variant_Part, Token_Ptr);
      Push_Scope_Stack;
      Scope.Table (Scope.Last).Etyp := E_Case;
      Scope.Table (Scope.Last).Sloc := Token_Ptr;
      Scope.Table (Scope.Last).Ecol := Start_Column;

      Scan; -- past CASE
      Case_Node := P_Expression;
      Set_Name (Variant_Part_Node, Case_Node);

      if Nkind (Case_Node) /= N_Identifier then
         Set_Name (Variant_Part_Node, Error);
         Error_Msg ("discriminant name expected", Sloc (Case_Node));

      elsif Paren_Count (Case_Node) /= 0 then
         Error_Msg
           ("|discriminant name may not be parenthesized",
                    Sloc (Case_Node));
         Set_Paren_Count (Case_Node, 0);
      end if;

      TF_Is;
      Variants_List := New_List;
      P_Pragmas_Opt (Variants_List);

      --  Test missing variant

      if Token = Tok_End then
         Error_Msg_BC ("WHEN expected (must have at least one variant)");
      else
         Append (P_Variant, Variants_List);
      end if;

      --  Loop through variants, note that we allow if in place of when,
      --  this error will be detected and handled in P_Variant.

      loop
         P_Pragmas_Opt (Variants_List);

         if Token /= Tok_When
           and then Token /= Tok_If
           and then Token /= Tok_Others
         then
            exit when Check_End;
         end if;

         Append (P_Variant, Variants_List);
      end loop;

      Set_Variants (Variant_Part_Node, Variants_List);
      return Variant_Part_Node;
   end P_Variant_Part;

   --------------------
   -- 3.8.1  Variant --
   --------------------

   --  VARIANT ::=
   --    when DISCRETE_CHOICE_LIST =>
   --      COMPONENT_LIST

   --  Error recovery: cannot raise Error_Resync

   --  The initial token on entry is either WHEN, IF or OTHERS

   function P_Variant return Node_Id is
      Variant_Node : Node_Id;

   begin
      --  Special check to recover nicely from use of IF in place of WHEN

      if Token = Tok_If then
         T_When;
         Scan; -- past IF
      else
         T_When;
      end if;

      Variant_Node := New_Node (N_Variant, Prev_Token_Ptr);
      Set_Discrete_Choices (Variant_Node, P_Discrete_Choice_List);
      TF_Arrow;
      Set_Component_List (Variant_Node, P_Component_List);
      return Variant_Node;
   end P_Variant;

   ---------------------------------
   -- 3.8.1  Discrete Choice List --
   ---------------------------------

   --  DISCRETE_CHOICE_LIST ::= DISCRETE_CHOICE {| DISCRETE_CHOICE}

   --  DISCRETE_CHOICE ::= EXPRESSION | DISCRETE_RANGE | others

   --  Note: in Ada 83, the expression must be a simple expression

   --  Error recovery: cannot raise Error_Resync

   function P_Discrete_Choice_List return List_Id is
      Choices     : List_Id;
      Expr_Node   : Node_Id;
      Choice_Node : Node_Id;

   begin
      Choices := New_List;
      loop
         if Token = Tok_Others then
            Append (New_Node (N_Others_Choice, Token_Ptr), Choices);
            Scan; -- past OTHERS

         else
            begin
               --  Scan out expression or range attribute

               Expr_Node := P_Expression_Or_Range_Attribute;
               Ignore (Tok_Right_Paren);

               if Token = Tok_Colon
                 and then Nkind (Expr_Node) = N_Identifier
               then
                  Error_Msg_SP ("label not permitted in this context");
                  Scan; -- past colon

               --  Range attribute

               elsif Expr_Form = EF_Range_Attr then
                  Append (Expr_Node, Choices);

               --  Explicit range

               elsif Token = Tok_Dot_Dot then
                  Check_Simple_Expression (Expr_Node);
                  Choice_Node := New_Node (N_Range, Token_Ptr);
                  Set_Low_Bound (Choice_Node, Expr_Node);
                  Scan; -- past ..
                  Expr_Node := P_Expression_No_Right_Paren;
                  Check_Simple_Expression (Expr_Node);
                  Set_High_Bound (Choice_Node, Expr_Node);
                  Append (Choice_Node, Choices);

               --  Simple name, must be subtype, so range allowed

               elsif Expr_Form = EF_Simple_Name then
                  if Token = Tok_Range then
                     Append (P_Subtype_Indication (Expr_Node), Choices);

                  elsif Token in Token_Class_Consk then
                     Error_Msg_SC
                       ("the only constraint allowed here " &
                        "is a range constraint");
                     Discard_Junk_Node (P_Constraint_Opt);
                     Append (Expr_Node, Choices);

                  else
                     Append (Expr_Node, Choices);
                  end if;

               --  Expression

               else
                  --  In Ada 2012 mode, the expression must be a simple
                  --  expression. The resaon for this restriction (i.e. going
                  --  back to the Ada 83 rule) is to avoid ambiguities when set
                  --  membership operations are allowed, consider the
                  --  following:

                  --     when A in 1 .. 10 | 12 =>

                  --  This is ambiguous without parentheses, so we require one
                  --  of the following two parenthesized forms to disambuguate:

                  --  one of the following:

                  --     when (A in 1 .. 10 | 12) =>
                  --     when (A in 1 .. 10) | 12 =>

                  --  To solve this, in Ada 2012 mode, we disallow the use of
                  --  membership operations in expressions in choices.

                  --  Technically in the grammar, the expression must match the
                  --  grammar for restricted expression.

                  if Ada_Version >= Ada_2012 then
                     Check_Restricted_Expression (Expr_Node);

                  --  In Ada 83 mode, the syntax required a simple expression

                  else
                     Check_Simple_Expression_In_Ada_83 (Expr_Node);
                  end if;

                  Append (Expr_Node, Choices);
               end if;

            exception
               when Error_Resync =>
                  Resync_Choice;
                  return Error_List;
            end;
         end if;

         if Token = Tok_Comma then
            Error_Msg_SC -- CODEFIX
              (""","" should be ""'|""");
         else
            exit when Token /= Tok_Vertical_Bar;
         end if;

         Scan; -- past | or comma
      end loop;

      return Choices;
   end P_Discrete_Choice_List;

   ----------------------------
   -- 3.8.1  Discrete Choice --
   ----------------------------

   --  Parsed by P_Discrete_Choice_List (3.8.1)

   ----------------------------------
   -- 3.9.1  Record Extension Part --
   ----------------------------------

   --  RECORD_EXTENSION_PART ::= with RECORD_DEFINITION

   --  Parsed by P_Derived_Type_Def_Or_Private_Ext_Decl (3.4)

   --------------------------------------
   -- 3.9.4  Interface Type Definition --
   --------------------------------------

   --  INTERFACE_TYPE_DEFINITION ::=
   --    [limited | task | protected | synchronized] interface
   --      [and INTERFACE_LIST]

   --  Error recovery: cannot raise Error_Resync

   function P_Interface_Type_Definition
     (Abstract_Present : Boolean) return Node_Id
   is
      Typedef_Node : Node_Id;

   begin
      if Ada_Version < Ada_2005 then
         Error_Msg_SP ("abstract interface is an Ada 2005 extension");
         Error_Msg_SP ("\unit must be compiled with -gnat05 switch");
      end if;

      if Abstract_Present then
         Error_Msg_SP
           ("ABSTRACT not allowed in interface type definition " &
            "(RM 3.9.4(2/2))");
      end if;

      Scan; -- past INTERFACE

      --  Ada 2005 (AI-345): In case of interfaces with a null list of
      --  interfaces we build a record_definition node.

      if Token = Tok_Semicolon or else Aspect_Specifications_Present then
         Typedef_Node := New_Node (N_Record_Definition, Token_Ptr);

         Set_Abstract_Present  (Typedef_Node);
         Set_Tagged_Present    (Typedef_Node);
         Set_Null_Present      (Typedef_Node);
         Set_Interface_Present (Typedef_Node);

      --  Ada 2005 (AI-251): In case of not-synchronized interfaces that have
      --  a list of interfaces we build a derived_type_definition node. This
      --  simplifies the semantic analysis (and hence further maintenance)

      else
         if Token /= Tok_And then
            Error_Msg_AP ("AND expected");
         else
            Scan; -- past AND
         end if;

         Typedef_Node := New_Node (N_Derived_Type_Definition, Token_Ptr);

         Set_Abstract_Present   (Typedef_Node);
         Set_Interface_Present  (Typedef_Node);
         Set_Subtype_Indication (Typedef_Node, P_Qualified_Simple_Name);

         Set_Record_Extension_Part (Typedef_Node,
           New_Node (N_Record_Definition, Token_Ptr));
         Set_Null_Present (Record_Extension_Part (Typedef_Node));

         if Token = Tok_And then
            Set_Interface_List (Typedef_Node, New_List);
            Scan; -- past AND

            loop
               Append (P_Qualified_Simple_Name,
                       Interface_List (Typedef_Node));
               exit when Token /= Tok_And;
               Scan; -- past AND
            end loop;
         end if;
      end if;

      return Typedef_Node;
   end P_Interface_Type_Definition;

   ----------------------------------
   -- 3.10  Access Type Definition --
   ----------------------------------

   --  ACCESS_TYPE_DEFINITION ::=
   --    ACCESS_TO_OBJECT_DEFINITION
   --  | ACCESS_TO_SUBPROGRAM_DEFINITION

   --  ACCESS_TO_OBJECT_DEFINITION ::=
   --    [NULL_EXCLUSION] access [GENERAL_ACCESS_MODIFIER] SUBTYPE_INDICATION

   --  GENERAL_ACCESS_MODIFIER ::= all | constant

   --  ACCESS_TO_SUBPROGRAM_DEFINITION
   --    [NULL_EXCLUSION] access [protected] procedure PARAMETER_PROFILE
   --  | [NULL_EXCLUSION] access [protected] function
   --    PARAMETER_AND_RESULT_PROFILE

   --  PARAMETER_PROFILE ::= [FORMAL_PART]

   --  PARAMETER_AND_RESULT_PROFILE ::= [FORMAL_PART] RETURN SUBTYPE_MARK

   --  Ada 2005 (AI-254): If Header_Already_Parsed then the caller has already
   --  parsed the null_exclusion part and has also removed the ACCESS token;
   --  otherwise the caller has just checked that the initial token is ACCESS

   --  Error recovery: can raise Error_Resync

   function P_Access_Type_Definition
     (Header_Already_Parsed : Boolean := False) return Node_Id
   is
      Access_Loc       : constant Source_Ptr := Token_Ptr;
      Prot_Flag        : Boolean;
      Not_Null_Present : Boolean := False;
      Type_Def_Node    : Node_Id;
      Result_Not_Null  : Boolean;
      Result_Node      : Node_Id;

      procedure Check_Junk_Subprogram_Name;
      --  Used in access to subprogram definition cases to check for an
      --  identifier or operator symbol that does not belong.

      --------------------------------
      -- Check_Junk_Subprogram_Name --
      --------------------------------

      procedure Check_Junk_Subprogram_Name is
         Saved_State : Saved_Scan_State;

      begin
         if Token = Tok_Identifier or else Token = Tok_Operator_Symbol then
            Save_Scan_State (Saved_State);
            Scan; -- past possible junk subprogram name

            if Token = Tok_Left_Paren or else Token = Tok_Semicolon then
               Error_Msg_SP ("unexpected subprogram name ignored");
               return;

            else
               Restore_Scan_State (Saved_State);
            end if;
         end if;
      end Check_Junk_Subprogram_Name;

   --  Start of processing for P_Access_Type_Definition

   begin
      if not Header_Already_Parsed then
         Not_Null_Present := P_Null_Exclusion;         --  Ada 2005 (AI-231)
         Scan; -- past ACCESS
      end if;

      if Token_Name = Name_Protected then
         Check_95_Keyword (Tok_Protected, Tok_Procedure);
         Check_95_Keyword (Tok_Protected, Tok_Function);
      end if;

      Prot_Flag := (Token = Tok_Protected);

      if Prot_Flag then
         Scan; -- past PROTECTED

         if Token /= Tok_Procedure and then Token /= Tok_Function then
            Error_Msg_SC -- CODEFIX
              ("FUNCTION or PROCEDURE expected");
         end if;
      end if;

      if Token = Tok_Procedure then
         if Ada_Version = Ada_83 then
            Error_Msg_SC ("(Ada 83) access to procedure not allowed!");
         end if;

         Type_Def_Node := New_Node (N_Access_Procedure_Definition, Access_Loc);
         Set_Null_Exclusion_Present (Type_Def_Node, Not_Null_Present);
         Scan; -- past PROCEDURE
         Check_Junk_Subprogram_Name;
         Set_Parameter_Specifications (Type_Def_Node, P_Parameter_Profile);
         Set_Protected_Present (Type_Def_Node, Prot_Flag);

      elsif Token = Tok_Function then
         if Ada_Version = Ada_83 then
            Error_Msg_SC ("(Ada 83) access to function not allowed!");
         end if;

         Type_Def_Node := New_Node (N_Access_Function_Definition, Access_Loc);
         Set_Null_Exclusion_Present (Type_Def_Node, Not_Null_Present);
         Scan; -- past FUNCTION
         Check_Junk_Subprogram_Name;
         Set_Parameter_Specifications (Type_Def_Node, P_Parameter_Profile);
         Set_Protected_Present (Type_Def_Node, Prot_Flag);
         TF_Return;

         Result_Not_Null := P_Null_Exclusion;     --  Ada 2005 (AI-231)

         --  Ada 2005 (AI-318-02)

         if Token = Tok_Access then
            if Ada_Version < Ada_2005 then
               Error_Msg_SC
                 ("anonymous access result type is an Ada 2005 extension");
               Error_Msg_SC ("\unit must be compiled with -gnat05 switch");
            end if;

            Result_Node := P_Access_Definition (Result_Not_Null);

         else
            Result_Node := P_Subtype_Mark;
            No_Constraint;

            --  A null exclusion on the result type must be recorded in a flag
            --  distinct from the one used for the access-to-subprogram type's
            --  null exclusion.

            Set_Null_Exclusion_In_Return_Present
              (Type_Def_Node, Result_Not_Null);
         end if;

         Set_Result_Definition (Type_Def_Node, Result_Node);

      else
         Type_Def_Node :=
           New_Node (N_Access_To_Object_Definition, Access_Loc);
         Set_Null_Exclusion_Present (Type_Def_Node, Not_Null_Present);

         if Token = Tok_All or else Token = Tok_Constant then
            if Ada_Version = Ada_83 then
               Error_Msg_SC ("(Ada 83) access modifier not allowed!");
            end if;

            if Token = Tok_All then
               Set_All_Present (Type_Def_Node, True);

            else
               Set_Constant_Present (Type_Def_Node, True);
            end if;

            Scan; -- past ALL or CONSTANT
         end if;

         Set_Subtype_Indication (Type_Def_Node,
            P_Subtype_Indication (Not_Null_Present));
      end if;

      return Type_Def_Node;
   end P_Access_Type_Definition;

   ---------------------------------------
   -- 3.10  Access To Object Definition --
   ---------------------------------------

   --  Parsed by P_Access_Type_Definition (3.10)

   -----------------------------------
   -- 3.10  General Access Modifier --
   -----------------------------------

   --  Parsed by P_Access_Type_Definition (3.10)

   -------------------------------------------
   -- 3.10  Access To Subprogram Definition --
   -------------------------------------------

   --  Parsed by P_Access_Type_Definition (3.10)

   -----------------------------
   -- 3.10  Access Definition --
   -----------------------------

   --  ACCESS_DEFINITION ::=
   --    [NULL_EXCLUSION] access [GENERAL_ACCESS_MODIFIER] SUBTYPE_MARK
   --  | ACCESS_TO_SUBPROGRAM_DEFINITION
   --
   --  ACCESS_TO_SUBPROGRAM_DEFINITION
   --    [NULL_EXCLUSION] access [protected] procedure PARAMETER_PROFILE
   --  | [NULL_EXCLUSION] access [protected] function
   --    PARAMETER_AND_RESULT_PROFILE

   --  The caller has parsed the null-exclusion part and it has also checked
   --  that the next token is ACCESS

   --  Error recovery: cannot raise Error_Resync

   function P_Access_Definition
     (Null_Exclusion_Present : Boolean) return Node_Id
   is
      Def_Node  : Node_Id;
      Subp_Node : Node_Id;

   begin
      Def_Node := New_Node (N_Access_Definition, Token_Ptr);
      Scan; -- past ACCESS

      --  Ada 2005 (AI-254): Access_To_Subprogram_Definition

      if Token = Tok_Protected
        or else Token = Tok_Procedure
        or else Token = Tok_Function
      then
         if Ada_Version < Ada_2005 then
            Error_Msg_SP ("access-to-subprogram is an Ada 2005 extension");
            Error_Msg_SP ("\unit should be compiled with -gnat05 switch");
         end if;

         Subp_Node := P_Access_Type_Definition (Header_Already_Parsed => True);
         Set_Null_Exclusion_Present (Subp_Node, Null_Exclusion_Present);
         Set_Access_To_Subprogram_Definition (Def_Node, Subp_Node);

      --  Ada 2005 (AI-231)
      --  [NULL_EXCLUSION] access [GENERAL_ACCESS_MODIFIER] SUBTYPE_MARK

      else
         Set_Null_Exclusion_Present (Def_Node, Null_Exclusion_Present);

         if Token = Tok_All then
            if Ada_Version < Ada_2005 then
               Error_Msg_SP
                 ("ALL is not permitted for anonymous access types");
            end if;

            Scan; -- past ALL
            Set_All_Present (Def_Node);

         elsif Token = Tok_Constant then
            if Ada_Version < Ada_2005 then
               Error_Msg_SP ("access-to-constant is an Ada 2005 extension");
               Error_Msg_SP ("\unit should be compiled with -gnat05 switch");
            end if;

            Scan; -- past CONSTANT
            Set_Constant_Present (Def_Node);
         end if;

         Set_Subtype_Mark (Def_Node, P_Subtype_Mark);
         No_Constraint;
      end if;

      return Def_Node;
   end P_Access_Definition;

   -----------------------------------------
   -- 3.10.1  Incomplete Type Declaration --
   -----------------------------------------

   --  Parsed by P_Type_Declaration (3.2.1)

   ----------------------------
   -- 3.11  Declarative Part --
   ----------------------------

   --  DECLARATIVE_PART ::= {DECLARATIVE_ITEM}

   --  Error recovery: cannot raise Error_Resync (because P_Declarative_Items
   --  handles errors, and returns cleanly after an error has occurred)

   function P_Declarative_Part return List_Id is
      Decls : List_Id;
      Done  : Boolean;

   begin
      --  Indicate no bad declarations detected yet. This will be reset by
      --  P_Declarative_Items if a bad declaration is discovered.

      Missing_Begin_Msg := No_Error_Msg;

      --  Get rid of active SIS entry from outer scope. This means we will
      --  miss some nested cases, but it doesn't seem worth the effort. See
      --  discussion in Par for further details

      SIS_Entry_Active := False;
      Decls := New_List;

      --  Loop to scan out the declarations

      loop
         P_Declarative_Items (Decls, Done, In_Spec => False);
         exit when Done;
      end loop;

      --  Get rid of active SIS entry which is left set only if we scanned a
      --  procedure declaration and have not found the body. We could give
      --  an error message, but that really would be usurping the role of
      --  semantic analysis (this really is a missing body case).

      SIS_Entry_Active := False;
      return Decls;
   end P_Declarative_Part;

   ----------------------------
   -- 3.11  Declarative Item --
   ----------------------------

   --  DECLARATIVE_ITEM ::= BASIC_DECLARATIVE_ITEM | BODY

   --  Can return Error if a junk declaration is found, or Empty if no
   --  declaration is found (i.e. a token ending declarations, such as
   --  BEGIN or END is encountered).

   --  Error recovery: cannot raise Error_Resync. If an error resync occurs,
   --  then the scan is set past the next semicolon and Error is returned.

   procedure P_Declarative_Items
     (Decls   : List_Id;
      Done    : out Boolean;
      In_Spec : Boolean)
   is
      Scan_State : Saved_Scan_State;

   begin
      if Style_Check then
         Style.Check_Indentation;
      end if;

      case Token is

         when Tok_Function =>
            Check_Bad_Layout;
            Append (P_Subprogram (Pf_Decl_Gins_Pbod_Rnam_Stub_Pexp), Decls);
            Done := False;

         when Tok_For =>
            Check_Bad_Layout;

            --  Check for loop (premature statement)

            Save_Scan_State (Scan_State);
            Scan; -- past FOR

            if Token = Tok_Identifier then
               Scan; -- past identifier

               if Token = Tok_In then
                  Restore_Scan_State (Scan_State);
                  Statement_When_Declaration_Expected (Decls, Done, In_Spec);
                  return;
               end if;
            end if;

            --  Not a loop, so must be rep clause

            Restore_Scan_State (Scan_State);
            Append (P_Representation_Clause, Decls);
            Done := False;

         when Tok_Generic =>
            Check_Bad_Layout;
            Append (P_Generic, Decls);
            Done := False;

         when Tok_Identifier =>
            Check_Bad_Layout;

            --  Special check for misuse of overriding not in Ada 2005 mode

            if Token_Name = Name_Overriding
              and then not Next_Token_Is (Tok_Colon)
            then
               Error_Msg_SC ("overriding indicator is an Ada 2005 extension");
               Error_Msg_SC ("\unit must be compiled with -gnat05 switch");

               Token := Tok_Overriding;
               Append (P_Subprogram (Pf_Decl_Gins_Pbod_Rnam_Stub_Pexp), Decls);
               Done := False;

            --  Normal case, no overriding, or overriding followed by colon

            else
               P_Identifier_Declarations (Decls, Done, In_Spec);
            end if;

         --  Ada2005: A subprogram declaration can start with "not" or
         --  "overriding". In older versions, "overriding" is handled
         --  like an identifier, with the appropriate messages.

         when Tok_Not =>
            Check_Bad_Layout;
            Append (P_Subprogram (Pf_Decl_Gins_Pbod_Rnam_Stub_Pexp), Decls);
            Done := False;

         when Tok_Overriding =>
            Check_Bad_Layout;
            Append (P_Subprogram (Pf_Decl_Gins_Pbod_Rnam_Stub_Pexp), Decls);
            Done := False;

         when Tok_Package =>
            Check_Bad_Layout;
            Append (P_Package (Pf_Decl_Gins_Pbod_Rnam_Stub_Pexp), Decls);
            Done := False;

         when Tok_Pragma =>
            Append (P_Pragma, Decls);
            Done := False;

         when Tok_Procedure =>
            Check_Bad_Layout;
            Append (P_Subprogram (Pf_Decl_Gins_Pbod_Rnam_Stub_Pexp), Decls);
            Done := False;

         when Tok_Protected =>
            Check_Bad_Layout;
            Scan; -- past PROTECTED
            Append (P_Protected, Decls);
            Done := False;

         when Tok_Subtype =>
            Check_Bad_Layout;
            Append (P_Subtype_Declaration, Decls);
            Done := False;

         when Tok_Task =>
            Check_Bad_Layout;
            Scan; -- past TASK
            Append (P_Task, Decls);
            Done := False;

         when Tok_Type =>
            Check_Bad_Layout;
            Append (P_Type_Declaration, Decls);
            Done := False;

         when Tok_Use =>
            Check_Bad_Layout;
            Append (P_Use_Clause, Decls);
            Done := False;

         when Tok_With =>
            Check_Bad_Layout;
            Error_Msg_SC ("WITH can only appear in context clause");
            raise Error_Resync;

         --  BEGIN terminates the scan of a sequence of declarations unless
         --  there is a missing subprogram body, see section on handling
         --  semicolon in place of IS. We only treat the begin as satisfying
         --  the subprogram declaration if it falls in the expected column
         --  or to its right.

         when Tok_Begin =>
            if SIS_Entry_Active and then Start_Column >= SIS_Ecol then

               --  Here we have the case where a BEGIN is encountered during
               --  declarations in a declarative part, or at the outer level,
               --  and there is a subprogram declaration outstanding for which
               --  no body has been supplied. This is the case where we assume
               --  that the semicolon in the subprogram declaration should
               --  really have been is. The active SIS entry describes the
               --  subprogram declaration. On return the declaration has been
               --  modified to become a body.

               declare
                  Specification_Node : Node_Id;
                  Decl_Node          : Node_Id;
                  Body_Node          : Node_Id;

               begin
                  --  First issue the error message. If we had a missing
                  --  semicolon in the declaration, then change the message
                  --  to <missing "is">

                  if SIS_Missing_Semicolon_Message /= No_Error_Msg then
                     Change_Error_Text     -- Replace: "missing "";"" "
                       (SIS_Missing_Semicolon_Message, "missing ""is""");

                  --  Otherwise we saved the semicolon position, so complain

                  else
                     Error_Msg -- CODEFIX
                       ("|"";"" should be IS", SIS_Semicolon_Sloc);
                  end if;

                  --  The next job is to fix up any declarations that occurred
                  --  between the procedure header and the BEGIN. These got
                  --  chained to the outer declarative region (immediately
                  --  after the procedure declaration) and they should be
                  --  chained to the subprogram itself, which is a body
                  --  rather than a spec.

                  Specification_Node := Specification (SIS_Declaration_Node);
                  Change_Node (SIS_Declaration_Node, N_Subprogram_Body);
                  Body_Node := SIS_Declaration_Node;
                  Set_Specification (Body_Node, Specification_Node);
                  Set_Declarations (Body_Node, New_List);

                  loop
                     Decl_Node := Remove_Next (Body_Node);
                     exit when Decl_Node = Empty;
                     Append (Decl_Node, Declarations (Body_Node));
                  end loop;

                  --  Now make the scope table entry for the Begin-End and
                  --  scan it out

                  Push_Scope_Stack;
                  Scope.Table (Scope.Last).Sloc := SIS_Sloc;
                  Scope.Table (Scope.Last).Etyp := E_Name;
                  Scope.Table (Scope.Last).Ecol := SIS_Ecol;
                  Scope.Table (Scope.Last).Labl := SIS_Labl;
                  Scope.Table (Scope.Last).Lreq := False;
                  SIS_Entry_Active := False;
                  Scan; -- past BEGIN
                  Set_Handled_Statement_Sequence (Body_Node,
                    P_Handled_Sequence_Of_Statements);
                  End_Statements (Handled_Statement_Sequence (Body_Node));
               end;

               Done := False;

            else
               Done := True;
            end if;

         --  Normally an END terminates the scan for basic declarative items.
         --  The one exception is END RECORD, which is probably left over from
         --  some other junk.

         when Tok_End =>
            Save_Scan_State (Scan_State); -- at END
            Scan; -- past END

            if Token = Tok_Record then
               Error_Msg_SP ("no RECORD for this `end record`!");
               Scan; -- past RECORD
               TF_Semicolon;

            else
               Restore_Scan_State (Scan_State); -- to END
               Done := True;
            end if;

         --  The following tokens which can only be the start of a statement
         --  are considered to end a declarative part (i.e. we have a missing
         --  BEGIN situation). We are fairly conservative in making this
         --  judgment, because it is a real mess to go into statement mode
         --  prematurely in response to a junk declaration.

         when Tok_Abort     |
              Tok_Accept    |
              Tok_Declare   |
              Tok_Delay     |
              Tok_Exit      |
              Tok_Goto      |
              Tok_If        |
              Tok_Loop      |
              Tok_Null      |
              Tok_Requeue   |
              Tok_Select    |
              Tok_While     =>

            --  But before we decide that it's a statement, let's check for
            --  a reserved word misused as an identifier.

            if Is_Reserved_Identifier then
               Save_Scan_State (Scan_State);
               Scan; -- past the token

               --  If reserved identifier not followed by colon or comma, then
               --  this is most likely an assignment statement to the bad id.

               if Token /= Tok_Colon and then Token /= Tok_Comma then
                  Restore_Scan_State (Scan_State);
                  Statement_When_Declaration_Expected (Decls, Done, In_Spec);
                  return;

               --  Otherwise we have a declaration of the bad id

               else
                  Restore_Scan_State (Scan_State);
                  Scan_Reserved_Identifier (Force_Msg => True);
                  P_Identifier_Declarations (Decls, Done, In_Spec);
               end if;

            --  If not reserved identifier, then it's definitely a statement

            else
               Statement_When_Declaration_Expected (Decls, Done, In_Spec);
               return;
            end if;

         --  The token RETURN may well also signal a missing BEGIN situation,
         --  however, we never let it end the declarative part, because it may
         --  also be part of a half-baked function declaration.

         when Tok_Return =>
            Error_Msg_SC ("misplaced RETURN statement");
            raise Error_Resync;

         --  PRIVATE definitely terminates the declarations in a spec,
         --  and is an error in a body.

         when Tok_Private =>
            if In_Spec then
               Done := True;
            else
               Error_Msg_SC ("PRIVATE not allowed in body");
               Scan; -- past PRIVATE
            end if;

         --  An end of file definitely terminates the declarations!

         when Tok_EOF =>
            Done := True;

         --  The remaining tokens do not end the scan, but cannot start a
         --  valid declaration, so we signal an error and resynchronize.
         --  But first check for misuse of a reserved identifier.

         when others =>

            --  Here we check for a reserved identifier

            if Is_Reserved_Identifier then
               Save_Scan_State (Scan_State);
               Scan; -- past the token

               if Token /= Tok_Colon and then Token /= Tok_Comma then
                  Restore_Scan_State (Scan_State);
                  Set_Declaration_Expected;
                  raise Error_Resync;
               else
                  Restore_Scan_State (Scan_State);
                  Scan_Reserved_Identifier (Force_Msg => True);
                  Check_Bad_Layout;
                  P_Identifier_Declarations (Decls, Done, In_Spec);
               end if;

            else
               Set_Declaration_Expected;
               raise Error_Resync;
            end if;
      end case;

   --  To resynchronize after an error, we scan to the next semicolon and
   --  return with Done = False, indicating that there may still be more
   --  valid declarations to come.

   exception
      when Error_Resync =>
         Resync_Past_Semicolon;
         Done := False;
   end P_Declarative_Items;

   ----------------------------------
   -- 3.11  Basic Declarative Item --
   ----------------------------------

   --  BASIC_DECLARATIVE_ITEM ::=
   --    BASIC_DECLARATION | REPRESENTATION_CLAUSE | USE_CLAUSE

   --  Scan zero or more basic declarative items

   --  Error recovery: cannot raise Error_Resync. If an error is detected, then
   --  the scan pointer is repositioned past the next semicolon, and the scan
   --  for declarative items continues.

   function P_Basic_Declarative_Items return List_Id is
      Decl  : Node_Id;
      Decls : List_Id;
      Kind  : Node_Kind;
      Done  : Boolean;

   begin
      --  Indicate no bad declarations detected yet in the current context:
      --  visible or private declarations of a package spec.

      Missing_Begin_Msg := No_Error_Msg;

      --  Get rid of active SIS entry from outer scope. This means we will
      --  miss some nested cases, but it doesn't seem worth the effort. See
      --  discussion in Par for further details

      SIS_Entry_Active := False;

      --  Loop to scan out declarations

      Decls := New_List;

      loop
         P_Declarative_Items (Decls, Done, In_Spec => True);
         exit when Done;
      end loop;

      --  Get rid of active SIS entry. This is set only if we have scanned a
      --  procedure declaration and have not found the body. We could give
      --  an error message, but that really would be usurping the role of
      --  semantic analysis (this really is a case of a missing body).

      SIS_Entry_Active := False;

      --  Test for assorted illegal declarations not diagnosed elsewhere

      Decl := First (Decls);

      while Present (Decl) loop
         Kind := Nkind (Decl);

         --  Test for body scanned, not acceptable as basic decl item

         if Kind = N_Subprogram_Body or else
            Kind = N_Package_Body or else
            Kind = N_Task_Body or else
            Kind = N_Protected_Body
         then
            Error_Msg ("proper body not allowed in package spec", Sloc (Decl));

         --  Test for body stub scanned, not acceptable as basic decl item

         elsif Kind in N_Body_Stub then
            Error_Msg ("body stub not allowed in package spec", Sloc (Decl));

         elsif Kind = N_Assignment_Statement then
            Error_Msg
              ("assignment statement not allowed in package spec",
                 Sloc (Decl));
         end if;

         Next (Decl);
      end loop;

      return Decls;
   end P_Basic_Declarative_Items;

   ----------------
   -- 3.11  Body --
   ----------------

   --  For proper body, see below
   --  For body stub, see 10.1.3

   -----------------------
   -- 3.11  Proper Body --
   -----------------------

   --  Subprogram body is parsed by P_Subprogram (6.1)
   --  Package body is parsed by P_Package (7.1)
   --  Task body is parsed by P_Task (9.1)
   --  Protected body is parsed by P_Protected (9.4)

   ------------------------------
   -- Set_Declaration_Expected --
   ------------------------------

   procedure Set_Declaration_Expected is
   begin
      Error_Msg_SC ("declaration expected");

      if Missing_Begin_Msg = No_Error_Msg then
         Missing_Begin_Msg := Get_Msg_Id;
      end if;
   end Set_Declaration_Expected;

   ----------------------
   -- Skip_Declaration --
   ----------------------

   procedure Skip_Declaration (S : List_Id) is
      Dummy_Done : Boolean;
      pragma Warnings (Off, Dummy_Done);
   begin
      P_Declarative_Items (S, Dummy_Done, False);
   end Skip_Declaration;

   -----------------------------------------
   -- Statement_When_Declaration_Expected --
   -----------------------------------------

   procedure Statement_When_Declaration_Expected
     (Decls   : List_Id;
      Done    : out Boolean;
      In_Spec : Boolean)
   is
   begin
      --  Case of second occurrence of statement in one declaration sequence

      if Missing_Begin_Msg /= No_Error_Msg then

         --  In the procedure spec case, just ignore it, we only give one
         --  message for the first occurrence, since otherwise we may get
         --  horrible cascading if BODY was missing in the header line.

         if In_Spec then
            null;

         --  In the declarative part case, take a second statement as a sure
         --  sign that we really have a missing BEGIN, and end the declarative
         --  part now. Note that the caller will fix up the first message to
         --  say "missing BEGIN" so that's how the error will be signalled.

         else
            Done := True;
            return;
         end if;

      --  Case of first occurrence of unexpected statement

      else
         --  If we are in a package spec, then give message of statement
         --  not allowed in package spec. This message never gets changed.

         if In_Spec then
            Error_Msg_SC ("statement not allowed in package spec");

         --  If in declarative part, then we give the message complaining
         --  about finding a statement when a declaration is expected. This
         --  gets changed to a complaint about a missing BEGIN if we later
         --  find that no BEGIN is present.

         else
            Error_Msg_SC ("statement not allowed in declarative part");
         end if;

         --  Capture message Id. This is used for two purposes, first to
         --  stop multiple messages, see test above, and second, to allow
         --  the replacement of the message in the declarative part case.

         Missing_Begin_Msg := Get_Msg_Id;
      end if;

      --  In all cases except the case in which we decided to terminate the
      --  declaration sequence on a second error, we scan out the statement
      --  and append it to the list of declarations (note that the semantics
      --  can handle statements in a declaration list so if we proceed to
      --  call the semantic phase, all will be (reasonably) well!

      Append_List_To (Decls, P_Sequence_Of_Statements (SS_Unco));

      --  Done is set to False, since we want to continue the scan of
      --  declarations, hoping that this statement was a temporary glitch.
      --  If we indeed are now in the statement part (i.e. this was a missing
      --  BEGIN, then it's not terrible, we will simply keep calling this
      --  procedure to process the statements one by one, and then finally
      --  hit the missing BEGIN, which will clean up the error message.

      Done := False;
   end Statement_When_Declaration_Expected;

end Ch3;
