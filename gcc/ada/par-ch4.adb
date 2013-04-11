------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P A R . C H 4                               --
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

with Stringt; use Stringt;

separate (Par)
package body Ch4 is

   --  Attributes that cannot have arguments

   Is_Parameterless_Attribute : constant Attribute_Class_Array :=
     (Attribute_Base         => True,
      Attribute_Body_Version => True,
      Attribute_Class        => True,
      Attribute_External_Tag => True,
      Attribute_Img          => True,
      Attribute_Stub_Type    => True,
      Attribute_Version      => True,
      Attribute_Type_Key     => True,
      others                 => False);
   --  This map contains True for parameterless attributes that return a
   --  string or a type. For those attributes, a left parenthesis after
   --  the attribute should not be analyzed as the beginning of a parameters
   --  list because it may denote a slice operation (X'Img (1 .. 2)) or
   --  a type conversion (X'Class (Y)).

   --  Note that this map designates the minimum set of attributes where a
   --  construct in parentheses that is not an argument can appear right
   --  after the attribute. For attributes like 'Size, we do not put them
   --  in the map. If someone writes X'Size (3), that's illegal in any case,
   --  but we get a better error message by parsing the (3) as an illegal
   --  argument to the attribute, rather than some meaningless junk that
   --  follows the attribute.

   -----------------------
   -- Local Subprograms --
   -----------------------

   function P_Aggregate_Or_Paren_Expr                 return Node_Id;
   function P_Allocator                               return Node_Id;
   function P_Case_Expression_Alternative             return Node_Id;
   function P_Record_Or_Array_Component_Association   return Node_Id;
   function P_Factor                                  return Node_Id;
   function P_Primary                                 return Node_Id;
   function P_Relation                                return Node_Id;
   function P_Term                                    return Node_Id;

   function P_Binary_Adding_Operator                  return Node_Kind;
   function P_Logical_Operator                        return Node_Kind;
   function P_Multiplying_Operator                    return Node_Kind;
   function P_Relational_Operator                     return Node_Kind;
   function P_Unary_Adding_Operator                   return Node_Kind;

   procedure Bad_Range_Attribute (Loc : Source_Ptr);
   --  Called to place complaint about bad range attribute at the given
   --  source location. Terminates by raising Error_Resync.

   procedure Check_Bad_Exp;
   --  Called after scanning a**b, posts error if ** detected

   procedure P_Membership_Test (N : Node_Id);
   --  N is the node for a N_In or N_Not_In node whose right operand has not
   --  yet been processed. It is called just after scanning out the IN keyword.
   --  On return, either Right_Opnd or Alternatives is set, as appropriate.

   function P_Range_Attribute_Reference (Prefix_Node : Node_Id) return Node_Id;
   --  Scan a range attribute reference. The caller has scanned out the
   --  prefix. The current token is known to be an apostrophe and the
   --  following token is known to be RANGE.

   function P_Unparen_Cond_Case_Quant_Expression return Node_Id;
   --  This function is called with Token pointing to IF, CASE, or FOR, in a
   --  context that allows a case, conditional, or quantified expression if
   --  it is surrounded by parentheses. If not surrounded by parentheses, the
   --  expression is still returned, but an error message is issued.

   -------------------------
   -- Bad_Range_Attribute --
   -------------------------

   procedure Bad_Range_Attribute (Loc : Source_Ptr) is
   begin
      Error_Msg ("range attribute cannot be used in expression!", Loc);
      Resync_Expression;
   end Bad_Range_Attribute;

   -------------------
   -- Check_Bad_Exp --
   -------------------

   procedure Check_Bad_Exp is
   begin
      if Token = Tok_Double_Asterisk then
         Error_Msg_SC ("parenthesization required for '*'*");
         Scan; -- past **
         Discard_Junk_Node (P_Primary);
         Check_Bad_Exp;
      end if;
   end Check_Bad_Exp;

   --------------------------
   -- 4.1  Name (also 6.4) --
   --------------------------

   --  NAME ::=
   --    DIRECT_NAME        | EXPLICIT_DEREFERENCE
   --  | INDEXED_COMPONENT  | SLICE
   --  | SELECTED_COMPONENT | ATTRIBUTE
   --  | TYPE_CONVERSION    | FUNCTION_CALL
   --  | CHARACTER_LITERAL

   --  DIRECT_NAME ::= IDENTIFIER | OPERATOR_SYMBOL

   --  PREFIX ::= NAME | IMPLICIT_DEREFERENCE

   --  EXPLICIT_DEREFERENCE ::= NAME . all

   --  IMPLICIT_DEREFERENCE ::= NAME

   --  INDEXED_COMPONENT ::= PREFIX (EXPRESSION {, EXPRESSION})

   --  SLICE ::= PREFIX (DISCRETE_RANGE)

   --  SELECTED_COMPONENT ::= PREFIX . SELECTOR_NAME

   --  SELECTOR_NAME ::= IDENTIFIER | CHARACTER_LITERAL | OPERATOR_SYMBOL

   --  ATTRIBUTE_REFERENCE ::= PREFIX ' ATTRIBUTE_DESIGNATOR

   --  ATTRIBUTE_DESIGNATOR ::=
   --    IDENTIFIER [(static_EXPRESSION)]
   --  | access | delta | digits

   --  FUNCTION_CALL ::=
   --    function_NAME
   --  | function_PREFIX ACTUAL_PARAMETER_PART

   --  ACTUAL_PARAMETER_PART ::=
   --    (PARAMETER_ASSOCIATION {,PARAMETER_ASSOCIATION})

   --  PARAMETER_ASSOCIATION ::=
   --    [formal_parameter_SELECTOR_NAME =>] EXPLICIT_ACTUAL_PARAMETER

   --  EXPLICIT_ACTUAL_PARAMETER ::= EXPRESSION | variable_NAME

   --  Note: syntactically a procedure call looks just like a function call,
   --  so this routine is in practice used to scan out procedure calls as well.

   --  On return, Expr_Form is set to either EF_Name or EF_Simple_Name

   --  Error recovery: can raise Error_Resync

   --  Note: if on return Token = Tok_Apostrophe, then the apostrophe must be
   --  followed by either a left paren (qualified expression case), or by
   --  range (range attribute case). All other uses of apostrophe (i.e. all
   --  other attributes) are handled in this routine.

   --  Error recovery: can raise Error_Resync

   function P_Name return Node_Id is
      Scan_State  : Saved_Scan_State;
      Name_Node   : Node_Id;
      Prefix_Node : Node_Id;
      Ident_Node  : Node_Id;
      Expr_Node   : Node_Id;
      Range_Node  : Node_Id;
      Arg_Node    : Node_Id;

      Arg_List  : List_Id := No_List; -- kill junk warning
      Attr_Name : Name_Id := No_Name; -- kill junk warning

   begin
      --  Case of not a name

      if Token not in Token_Class_Name then

         --  If it looks like start of expression, complain and scan expression

         if Token in Token_Class_Literal
           or else Token = Tok_Left_Paren
         then
            Error_Msg_SC ("name expected");
            return P_Expression;

         --  Otherwise some other junk, not much we can do

         else
            Error_Msg_AP ("name expected");
            raise Error_Resync;
         end if;
      end if;

      --  Loop through designators in qualified name

      Name_Node := Token_Node;

      loop
         Scan; -- past designator
         exit when Token /= Tok_Dot;
         Save_Scan_State (Scan_State); -- at dot
         Scan; -- past dot

         --  If we do not have another designator after the dot, then join
         --  the normal circuit to handle a dot extension (may be .all or
         --  character literal case). Otherwise loop back to scan the next
         --  designator.

         if Token not in Token_Class_Desig then
            goto Scan_Name_Extension_Dot;
         else
            Prefix_Node := Name_Node;
            Name_Node := New_Node (N_Selected_Component, Prev_Token_Ptr);
            Set_Prefix (Name_Node, Prefix_Node);
            Set_Selector_Name (Name_Node, Token_Node);
         end if;
      end loop;

      --  We have now scanned out a qualified designator. If the last token is
      --  an operator symbol, then we certainly do not have the Snam case, so
      --  we can just use the normal name extension check circuit

      if Prev_Token = Tok_Operator_Symbol then
         goto Scan_Name_Extension;
      end if;

      --  We have scanned out a qualified simple name, check for name extension
      --  Note that we know there is no dot here at this stage, so the only
      --  possible cases of name extension are apostrophe and left paren.

      if Token = Tok_Apostrophe then
         Save_Scan_State (Scan_State); -- at apostrophe
         Scan; -- past apostrophe

         --  Qualified expression in Ada 2012 mode (treated as a name)

         if Ada_Version >= Ada_2012 and then Token = Tok_Left_Paren then
            goto Scan_Name_Extension_Apostrophe;

         --  If left paren not in Ada 2012, then it is not part of the name,
         --  since qualified expressions are not names in prior versions of
         --  Ada, so return with Token backed up to point to the apostrophe.
         --  The treatment for the range attribute is similar (we do not
         --  consider x'range to be a name in this grammar).

         elsif Token = Tok_Left_Paren or else Token = Tok_Range then
            Restore_Scan_State (Scan_State); -- to apostrophe
            Expr_Form := EF_Simple_Name;
            return Name_Node;

         --  Otherwise we have the case of a name extended by an attribute

         else
            goto Scan_Name_Extension_Apostrophe;
         end if;

      --  Check case of qualified simple name extended by a left parenthesis

      elsif Token = Tok_Left_Paren then
         Scan; -- past left paren
         goto Scan_Name_Extension_Left_Paren;

      --  Otherwise the qualified simple name is not extended, so return

      else
         Expr_Form := EF_Simple_Name;
         return Name_Node;
      end if;

      --  Loop scanning past name extensions. A label is used for control
      --  transfer for this loop for ease of interfacing with the finite state
      --  machine in the parenthesis scanning circuit, and also to allow for
      --  passing in control to the appropriate point from the above code.

      <<Scan_Name_Extension>>

         --  Character literal used as name cannot be extended. Also this
         --  cannot be a call, since the name for a call must be a designator.
         --  Return in these cases, or if there is no name extension

         if Token not in Token_Class_Namext
           or else Prev_Token = Tok_Char_Literal
         then
            Expr_Form := EF_Name;
            return Name_Node;
         end if;

      --  Merge here when we know there is a name extension

      <<Scan_Name_Extension_OK>>

         if Token = Tok_Left_Paren then
            Scan; -- past left paren
            goto Scan_Name_Extension_Left_Paren;

         elsif Token = Tok_Apostrophe then
            Save_Scan_State (Scan_State); -- at apostrophe
            Scan; -- past apostrophe
            goto Scan_Name_Extension_Apostrophe;

         else -- Token = Tok_Dot
            Save_Scan_State (Scan_State); -- at dot
            Scan; -- past dot
            goto Scan_Name_Extension_Dot;
         end if;

      --  Case of name extended by dot (selection), dot is already skipped
      --  and the scan state at the point of the dot is saved in Scan_State.

      <<Scan_Name_Extension_Dot>>

         --  Explicit dereference case

         if Token = Tok_All then
            Prefix_Node := Name_Node;
            Name_Node := New_Node (N_Explicit_Dereference, Token_Ptr);
            Set_Prefix (Name_Node, Prefix_Node);
            Scan; -- past ALL
            goto Scan_Name_Extension;

         --  Selected component case

         elsif Token in Token_Class_Name then
            Prefix_Node := Name_Node;
            Name_Node := New_Node (N_Selected_Component, Prev_Token_Ptr);
            Set_Prefix (Name_Node, Prefix_Node);
            Set_Selector_Name (Name_Node, Token_Node);
            Scan; -- past selector
            goto Scan_Name_Extension;

         --  Reserved identifier as selector

         elsif Is_Reserved_Identifier then
            Scan_Reserved_Identifier (Force_Msg => False);
            Prefix_Node := Name_Node;
            Name_Node := New_Node (N_Selected_Component, Prev_Token_Ptr);
            Set_Prefix (Name_Node, Prefix_Node);
            Set_Selector_Name (Name_Node, Token_Node);
            Scan; -- past identifier used as selector
            goto Scan_Name_Extension;

         --  If dot is at end of line and followed by nothing legal,
         --  then assume end of name and quit (dot will be taken as
         --  an erroneous form of some other punctuation by our caller).

         elsif Token_Is_At_Start_Of_Line then
            Restore_Scan_State (Scan_State);
            return Name_Node;

         --  Here if nothing legal after the dot

         else
            Error_Msg_AP ("selector expected");
            raise Error_Resync;
         end if;

      --  Here for an apostrophe as name extension. The scan position at the
      --  apostrophe has already been saved, and the apostrophe scanned out.

      <<Scan_Name_Extension_Apostrophe>>

         Scan_Apostrophe : declare
            function Apostrophe_Should_Be_Semicolon return Boolean;
            --  Checks for case where apostrophe should probably be
            --  a semicolon, and if so, gives appropriate message,
            --  resets the scan pointer to the apostrophe, changes
            --  the current token to Tok_Semicolon, and returns True.
            --  Otherwise returns False.

            ------------------------------------
            -- Apostrophe_Should_Be_Semicolon --
            ------------------------------------

            function Apostrophe_Should_Be_Semicolon return Boolean is
            begin
               if Token_Is_At_Start_Of_Line then
                  Restore_Scan_State (Scan_State); -- to apostrophe
                  Error_Msg_SC ("|""''"" should be "";""");
                  Token := Tok_Semicolon;
                  return True;
               else
                  return False;
               end if;
            end Apostrophe_Should_Be_Semicolon;

         --  Start of processing for Scan_Apostrophe

         begin
            --  Check for qualified expression case in Ada 2012 mode

            if Ada_Version >= Ada_2012 and then Token = Tok_Left_Paren then
               Name_Node := P_Qualified_Expression (Name_Node);
               goto Scan_Name_Extension;

            --  If range attribute after apostrophe, then return with Token
            --  pointing to the apostrophe. Note that in this case the prefix
            --  need not be a simple name (cases like A.all'range). Similarly
            --  if there is a left paren after the apostrophe, then we also
            --  return with Token pointing to the apostrophe (this is the
            --  aggregate case, or some error case).

            elsif Token = Tok_Range or else Token = Tok_Left_Paren then
               Restore_Scan_State (Scan_State); -- to apostrophe
               Expr_Form := EF_Name;
               return Name_Node;

            --  Here for cases where attribute designator is an identifier

            elsif Token = Tok_Identifier then
               Attr_Name := Token_Name;

               if not Is_Attribute_Name (Attr_Name) then
                  if Apostrophe_Should_Be_Semicolon then
                     Expr_Form := EF_Name;
                     return Name_Node;

                  --  Here for a bad attribute name

                  else
                     Signal_Bad_Attribute;
                     Scan; -- past bad identifier

                     if Token = Tok_Left_Paren then
                        Scan; -- past left paren

                        loop
                           Discard_Junk_Node (P_Expression_If_OK);
                           exit when not  Comma_Present;
                        end loop;

                        T_Right_Paren;
                     end if;

                     return Error;
                  end if;
               end if;

               if Style_Check then
                  Style.Check_Attribute_Name (False);
               end if;

            --  Here for case of attribute designator is not an identifier

            else
               if Token = Tok_Delta then
                  Attr_Name := Name_Delta;

               elsif Token = Tok_Digits then
                  Attr_Name := Name_Digits;

               elsif Token = Tok_Access then
                  Attr_Name := Name_Access;

               elsif Token = Tok_Mod and then Ada_Version >= Ada_95 then
                  Attr_Name := Name_Mod;

               elsif Apostrophe_Should_Be_Semicolon then
                  Expr_Form := EF_Name;
                  return Name_Node;

               else
                  Error_Msg_AP ("attribute designator expected");
                  raise Error_Resync;
               end if;

               if Style_Check then
                  Style.Check_Attribute_Name (True);
               end if;
            end if;

            --  We come here with an OK attribute scanned, and corresponding
            --  Attribute identifier node stored in Ident_Node.

            Prefix_Node := Name_Node;
            Name_Node := New_Node (N_Attribute_Reference, Prev_Token_Ptr);
            Scan; -- past attribute designator
            Set_Prefix (Name_Node, Prefix_Node);
            Set_Attribute_Name (Name_Node, Attr_Name);

            --  Scan attribute arguments/designator. We skip this if we know
            --  that the attribute cannot have an argument.

            if Token = Tok_Left_Paren
              and then not
                Is_Parameterless_Attribute (Get_Attribute_Id (Attr_Name))
            then
               Set_Expressions (Name_Node, New_List);

               --  Attribute Update contains an array or record association
               --  list which provides new values for various components or
               --  elements. The list is parsed as an aggregate.

               if Attr_Name = Name_Update then
                  Append (P_Aggregate, Expressions (Name_Node));

               else
                  Scan; -- past left paren

                  loop
                     declare
                        Expr : constant Node_Id := P_Expression_If_OK;

                     begin
                        if Token = Tok_Arrow then
                           Error_Msg_SC
                             ("named parameters not permitted for attributes");
                           Scan; -- past junk arrow

                        else
                           Append (Expr, Expressions (Name_Node));
                           exit when not Comma_Present;
                        end if;
                     end;
                  end loop;

                  T_Right_Paren;
               end if;
            end if;

            goto Scan_Name_Extension;
         end Scan_Apostrophe;

      --  Here for left parenthesis extending name (left paren skipped)

      <<Scan_Name_Extension_Left_Paren>>

         --  We now have to scan through a list of items, terminated by a
         --  right parenthesis. The scan is handled by a finite state
         --  machine. The possibilities are:

         --   (discrete_range)

         --      This is a slice. This case is handled in LP_State_Init

         --   (expression, expression, ..)

         --      This is interpreted as an indexed component, i.e. as a
         --      case of a name which can be extended in the normal manner.
         --      This case is handled by LP_State_Name or LP_State_Expr.

         --      Note: if and case expressions (without an extra level of
         --      parentheses) are permitted in this context).

         --   (..., identifier => expression , ...)

         --      If there is at least one occurrence of identifier => (but
         --      none of the other cases apply), then we have a call.

         --  Test for Id => case

         if Token = Tok_Identifier then
            Save_Scan_State (Scan_State); -- at Id
            Scan; -- past Id

            --  Test for => (allow := as an error substitute)

            if Token = Tok_Arrow or else Token = Tok_Colon_Equal then
               Restore_Scan_State (Scan_State); -- to Id
               Arg_List := New_List;
               goto LP_State_Call;

            else
               Restore_Scan_State (Scan_State); -- to Id
            end if;
         end if;

         --  Here we have an expression after all

         Expr_Node := P_Expression_Or_Range_Attribute_If_OK;

         --  Check cases of discrete range for a slice

         --  First possibility: Range_Attribute_Reference

         if Expr_Form = EF_Range_Attr then
            Range_Node := Expr_Node;

         --  Second possibility: Simple_expression .. Simple_expression

         elsif Token = Tok_Dot_Dot then
            Check_Simple_Expression (Expr_Node);
            Range_Node := New_Node (N_Range, Token_Ptr);
            Set_Low_Bound (Range_Node, Expr_Node);
            Scan; -- past ..
            Expr_Node := P_Expression;
            Check_Simple_Expression (Expr_Node);
            Set_High_Bound (Range_Node, Expr_Node);

         --  Third possibility: Type_name range Range

         elsif Token = Tok_Range then
            if Expr_Form /= EF_Simple_Name then
               Error_Msg_SC ("subtype mark must precede RANGE");
               raise Error_Resync;
            end if;

            Range_Node := P_Subtype_Indication (Expr_Node);

         --  Otherwise we just have an expression. It is true that we might
         --  have a subtype mark without a range constraint but this case
         --  is syntactically indistinguishable from the expression case.

         else
            Arg_List := New_List;
            goto LP_State_Expr;
         end if;

         --  Fall through here with unmistakable Discrete range scanned,
         --  which means that we definitely have the case of a slice. The
         --  Discrete range is in Range_Node.

         if Token = Tok_Comma then
            Error_Msg_SC ("slice cannot have more than one dimension");
            raise Error_Resync;

         elsif Token /= Tok_Right_Paren then
            if Token = Tok_Arrow then

               --  This may be an aggregate that is missing a qualification

               Error_Msg_SC
                 ("context of aggregate must be a qualified expression");
               raise Error_Resync;

            else
               T_Right_Paren;
               raise Error_Resync;
            end if;

         else
            Scan; -- past right paren
            Prefix_Node := Name_Node;
            Name_Node := New_Node (N_Slice, Sloc (Prefix_Node));
            Set_Prefix (Name_Node, Prefix_Node);
            Set_Discrete_Range (Name_Node, Range_Node);

            --  An operator node is legal as a prefix to other names,
            --  but not for a slice.

            if Nkind (Prefix_Node) = N_Operator_Symbol then
               Error_Msg_N ("illegal prefix for slice", Prefix_Node);
            end if;

            --  If we have a name extension, go scan it

            if Token in Token_Class_Namext then
               goto Scan_Name_Extension_OK;

            --  Otherwise return (a slice is a name, but is not a call)

            else
               Expr_Form := EF_Name;
               return Name_Node;
            end if;
         end if;

      --  In LP_State_Expr, we have scanned one or more expressions, and
      --  so we have a call or an indexed component which is a name. On
      --  entry we have the expression just scanned in Expr_Node and
      --  Arg_List contains the list of expressions encountered so far

      <<LP_State_Expr>>
         Append (Expr_Node, Arg_List);

         if Token = Tok_Arrow then
            Error_Msg
              ("expect identifier in parameter association",
                Sloc (Expr_Node));
            Scan;  -- past arrow

         elsif not Comma_Present then
            T_Right_Paren;
            Prefix_Node := Name_Node;
            Name_Node := New_Node (N_Indexed_Component, Sloc (Prefix_Node));
            Set_Prefix (Name_Node, Prefix_Node);
            Set_Expressions (Name_Node, Arg_List);
            goto Scan_Name_Extension;
         end if;

         --  Comma present (and scanned out), test for identifier => case
         --  Test for identifier => case

         if Token = Tok_Identifier then
            Save_Scan_State (Scan_State); -- at Id
            Scan; -- past Id

            --  Test for => (allow := as error substitute)

            if Token = Tok_Arrow or else Token = Tok_Colon_Equal then
               Restore_Scan_State (Scan_State); -- to Id
               goto LP_State_Call;

            --  Otherwise it's just an expression after all, so backup

            else
               Restore_Scan_State (Scan_State); -- to Id
            end if;
         end if;

         --  Here we have an expression after all, so stay in this state

         Expr_Node := P_Expression_If_OK;
         goto LP_State_Expr;

      --  LP_State_Call corresponds to the situation in which at least
      --  one instance of Id => Expression has been encountered, so we
      --  know that we do not have a name, but rather a call. We enter
      --  it with the scan pointer pointing to the next argument to scan,
      --  and Arg_List containing the list of arguments scanned so far.

      <<LP_State_Call>>

         --  Test for case of Id => Expression (named parameter)

         if Token = Tok_Identifier then
            Save_Scan_State (Scan_State); -- at Id
            Ident_Node := Token_Node;
            Scan; -- past Id

            --  Deal with => (allow := as erroneous substitute)

            if Token = Tok_Arrow or else Token = Tok_Colon_Equal then
               Arg_Node := New_Node (N_Parameter_Association, Prev_Token_Ptr);
               Set_Selector_Name (Arg_Node, Ident_Node);
               T_Arrow;
               Set_Explicit_Actual_Parameter (Arg_Node, P_Expression);
               Append (Arg_Node, Arg_List);

               --  If a comma follows, go back and scan next entry

               if Comma_Present then
                  goto LP_State_Call;

               --  Otherwise we have the end of a call

               else
                  Prefix_Node := Name_Node;
                  Name_Node := New_Node (N_Function_Call, Sloc (Prefix_Node));
                  Set_Name (Name_Node, Prefix_Node);
                  Set_Parameter_Associations (Name_Node, Arg_List);
                  T_Right_Paren;

                  if Token in Token_Class_Namext then
                     goto Scan_Name_Extension_OK;

                  --  This is a case of a call which cannot be a name

                  else
                     Expr_Form := EF_Name;
                     return Name_Node;
                  end if;
               end if;

            --  Not named parameter: Id started an expression after all

            else
               Restore_Scan_State (Scan_State); -- to Id
            end if;
         end if;

         --  Here if entry did not start with Id => which means that it
         --  is a positional parameter, which is not allowed, since we
         --  have seen at least one named parameter already.

         Error_Msg_SC
            ("positional parameter association " &
              "not allowed after named one");

         Expr_Node := P_Expression_If_OK;

         --  Leaving the '>' in an association is not unusual, so suggest
         --  a possible fix.

         if Nkind (Expr_Node) = N_Op_Eq then
            Error_Msg_N ("\maybe `='>` was intended", Expr_Node);
         end if;

         --  We go back to scanning out expressions, so that we do not get
         --  multiple error messages when several positional parameters
         --  follow a named parameter.

         goto LP_State_Expr;

         --  End of treatment for name extensions starting with left paren

      --  End of loop through name extensions

   end P_Name;

   --  This function parses a restricted form of Names which are either
   --  designators, or designators preceded by a sequence of prefixes
   --  that are direct names.

   --  Error recovery: cannot raise Error_Resync

   function P_Function_Name return Node_Id is
      Designator_Node : Node_Id;
      Prefix_Node     : Node_Id;
      Selector_Node   : Node_Id;
      Dot_Sloc        : Source_Ptr := No_Location;

   begin
      --  Prefix_Node is set to the gathered prefix so far, Empty means that
      --  no prefix has been scanned. This allows us to build up the result
      --  in the required right recursive manner.

      Prefix_Node := Empty;

      --  Loop through prefixes

      loop
         Designator_Node := Token_Node;

         if Token not in Token_Class_Desig then
            return P_Identifier; -- let P_Identifier issue the error message

         else -- Token in Token_Class_Desig
            Scan; -- past designator
            exit when Token /= Tok_Dot;
         end if;

         --  Here at a dot, with token just before it in Designator_Node

         if No (Prefix_Node) then
            Prefix_Node := Designator_Node;
         else
            Selector_Node := New_Node (N_Selected_Component, Dot_Sloc);
            Set_Prefix (Selector_Node, Prefix_Node);
            Set_Selector_Name (Selector_Node, Designator_Node);
            Prefix_Node := Selector_Node;
         end if;

         Dot_Sloc := Token_Ptr;
         Scan; -- past dot
      end loop;

      --  Fall out of the loop having just scanned a designator

      if No (Prefix_Node) then
         return Designator_Node;
      else
         Selector_Node := New_Node (N_Selected_Component, Dot_Sloc);
         Set_Prefix (Selector_Node, Prefix_Node);
         Set_Selector_Name (Selector_Node, Designator_Node);
         return Selector_Node;
      end if;

   exception
      when Error_Resync =>
         return Error;
   end P_Function_Name;

   --  This function parses a restricted form of Names which are either
   --  identifiers, or identifiers preceded by a sequence of prefixes
   --  that are direct names.

   --  Error recovery: cannot raise Error_Resync

   function P_Qualified_Simple_Name return Node_Id is
      Designator_Node : Node_Id;
      Prefix_Node     : Node_Id;
      Selector_Node   : Node_Id;
      Dot_Sloc        : Source_Ptr := No_Location;

   begin
      --  Prefix node is set to the gathered prefix so far, Empty means that
      --  no prefix has been scanned. This allows us to build up the result
      --  in the required right recursive manner.

      Prefix_Node := Empty;

      --  Loop through prefixes

      loop
         Designator_Node := Token_Node;

         if Token = Tok_Identifier then
            Scan; -- past identifier
            exit when Token /= Tok_Dot;

         elsif Token not in Token_Class_Desig then
            return P_Identifier; -- let P_Identifier issue the error message

         else
            Scan; -- past designator

            if Token /= Tok_Dot then
               Error_Msg_SP ("identifier expected");
               return Error;
            end if;
         end if;

         --  Here at a dot, with token just before it in Designator_Node

         if No (Prefix_Node) then
            Prefix_Node := Designator_Node;
         else
            Selector_Node := New_Node (N_Selected_Component, Dot_Sloc);
            Set_Prefix (Selector_Node, Prefix_Node);
            Set_Selector_Name (Selector_Node, Designator_Node);
            Prefix_Node := Selector_Node;
         end if;

         Dot_Sloc := Token_Ptr;
         Scan; -- past dot
      end loop;

      --  Fall out of the loop having just scanned an identifier

      if No (Prefix_Node) then
         return Designator_Node;
      else
         Selector_Node := New_Node (N_Selected_Component, Dot_Sloc);
         Set_Prefix (Selector_Node, Prefix_Node);
         Set_Selector_Name (Selector_Node, Designator_Node);
         return Selector_Node;
      end if;

   exception
      when Error_Resync =>
         return Error;
   end P_Qualified_Simple_Name;

   --  This procedure differs from P_Qualified_Simple_Name only in that it
   --  raises Error_Resync if any error is encountered. It only returns after
   --  scanning a valid qualified simple name.

   --  Error recovery: can raise Error_Resync

   function P_Qualified_Simple_Name_Resync return Node_Id is
      Designator_Node : Node_Id;
      Prefix_Node     : Node_Id;
      Selector_Node   : Node_Id;
      Dot_Sloc        : Source_Ptr := No_Location;

   begin
      Prefix_Node := Empty;

      --  Loop through prefixes

      loop
         Designator_Node := Token_Node;

         if Token = Tok_Identifier then
            Scan; -- past identifier
            exit when Token /= Tok_Dot;

         elsif Token not in Token_Class_Desig then
            Discard_Junk_Node (P_Identifier); -- to issue the error message
            raise Error_Resync;

         else
            Scan; -- past designator

            if Token /= Tok_Dot then
               Error_Msg_SP ("identifier expected");
               raise Error_Resync;
            end if;
         end if;

         --  Here at a dot, with token just before it in Designator_Node

         if No (Prefix_Node) then
            Prefix_Node := Designator_Node;
         else
            Selector_Node := New_Node (N_Selected_Component, Dot_Sloc);
            Set_Prefix (Selector_Node, Prefix_Node);
            Set_Selector_Name (Selector_Node, Designator_Node);
            Prefix_Node := Selector_Node;
         end if;

         Dot_Sloc := Token_Ptr;
         Scan; -- past period
      end loop;

      --  Fall out of the loop having just scanned an identifier

      if No (Prefix_Node) then
         return Designator_Node;
      else
         Selector_Node := New_Node (N_Selected_Component, Dot_Sloc);
         Set_Prefix (Selector_Node, Prefix_Node);
         Set_Selector_Name (Selector_Node, Designator_Node);
         return Selector_Node;
      end if;
   end P_Qualified_Simple_Name_Resync;

   ----------------------
   -- 4.1  Direct_Name --
   ----------------------

   --  Parsed by P_Name and other functions in section 4.1

   -----------------
   -- 4.1  Prefix --
   -----------------

   --  Parsed by P_Name (4.1)

   -------------------------------
   -- 4.1  Explicit Dereference --
   -------------------------------

   --  Parsed by P_Name (4.1)

   -------------------------------
   -- 4.1  Implicit_Dereference --
   -------------------------------

   --  Parsed by P_Name (4.1)

   ----------------------------
   -- 4.1  Indexed Component --
   ----------------------------

   --  Parsed by P_Name (4.1)

   ----------------
   -- 4.1  Slice --
   ----------------

   --  Parsed by P_Name (4.1)

   -----------------------------
   -- 4.1  Selected_Component --
   -----------------------------

   --  Parsed by P_Name (4.1)

   ------------------------
   -- 4.1  Selector Name --
   ------------------------

   --  Parsed by P_Name (4.1)

   ------------------------------
   -- 4.1  Attribute Reference --
   ------------------------------

   --  Parsed by P_Name (4.1)

   -------------------------------
   -- 4.1  Attribute Designator --
   -------------------------------

   --  Parsed by P_Name (4.1)

   --------------------------------------
   -- 4.1.4  Range Attribute Reference --
   --------------------------------------

   --  RANGE_ATTRIBUTE_REFERENCE ::= PREFIX ' RANGE_ATTRIBUTE_DESIGNATOR

   --  RANGE_ATTRIBUTE_DESIGNATOR ::= range [(static_EXPRESSION)]

   --  In the grammar, a RANGE attribute is simply a name, but its use is
   --  highly restricted, so in the parser, we do not regard it as a name.
   --  Instead, P_Name returns without scanning the 'RANGE part of the
   --  attribute, and the caller uses the following function to construct
   --  a range attribute in places where it is appropriate.

   --  Note that RANGE here is treated essentially as an identifier,
   --  rather than a reserved word.

   --  The caller has parsed the prefix, i.e. a name, and Token points to
   --  the apostrophe. The token after the apostrophe is known to be RANGE
   --  at this point. The prefix node becomes the prefix of the attribute.

   --  Error_Recovery: Cannot raise Error_Resync

   function P_Range_Attribute_Reference
     (Prefix_Node : Node_Id)
      return        Node_Id
   is
      Attr_Node  : Node_Id;

   begin
      Attr_Node := New_Node (N_Attribute_Reference, Token_Ptr);
      Set_Prefix (Attr_Node, Prefix_Node);
      Scan; -- past apostrophe

      if Style_Check then
         Style.Check_Attribute_Name (True);
      end if;

      Set_Attribute_Name (Attr_Node, Name_Range);
      Scan; -- past RANGE

      if Token = Tok_Left_Paren then
         Scan; -- past left paren
         Set_Expressions (Attr_Node, New_List (P_Expression_If_OK));
         T_Right_Paren;
      end if;

      return Attr_Node;
   end P_Range_Attribute_Reference;

   ---------------------------------------
   -- 4.1.4  Range Attribute Designator --
   ---------------------------------------

   --  Parsed by P_Range_Attribute_Reference (4.4)

   --------------------
   -- 4.3  Aggregate --
   --------------------

   --  AGGREGATE ::= RECORD_AGGREGATE | EXTENSION_AGGREGATE | ARRAY_AGGREGATE

   --  Parsed by P_Aggregate_Or_Paren_Expr (4.3), except in the case where
   --  an aggregate is known to be required (code statement, extension
   --  aggregate), in which cases this routine performs the necessary check
   --  that we have an aggregate rather than a parenthesized expression

   --  Error recovery: can raise Error_Resync

   function P_Aggregate return Node_Id is
      Aggr_Sloc : constant Source_Ptr := Token_Ptr;
      Aggr_Node : constant Node_Id    := P_Aggregate_Or_Paren_Expr;

   begin
      if Nkind (Aggr_Node) /= N_Aggregate
           and then
         Nkind (Aggr_Node) /= N_Extension_Aggregate
      then
         Error_Msg
           ("aggregate may not have single positional component", Aggr_Sloc);
         return Error;
      else
         return Aggr_Node;
      end if;
   end P_Aggregate;

   ------------------------------------------------
   -- 4.3  Aggregate or Parenthesized Expression --
   ------------------------------------------------

   --  This procedure parses out either an aggregate or a parenthesized
   --  expression (these two constructs are closely related, since a
   --  parenthesized expression looks like an aggregate with a single
   --  positional component).

   --  AGGREGATE ::=
   --    RECORD_AGGREGATE | EXTENSION_AGGREGATE | ARRAY_AGGREGATE

   --  RECORD_AGGREGATE ::= (RECORD_COMPONENT_ASSOCIATION_LIST)

   --  RECORD_COMPONENT_ASSOCIATION_LIST ::=
   --     RECORD_COMPONENT_ASSOCIATION {, RECORD_COMPONENT_ASSOCIATION}
   --   | null record

   --  RECORD_COMPONENT_ASSOCIATION ::=
   --    [COMPONENT_CHOICE_LIST =>] EXPRESSION

   --  COMPONENT_CHOICE_LIST ::=
   --    component_SELECTOR_NAME {| component_SELECTOR_NAME}
   --  | others

   --  EXTENSION_AGGREGATE ::=
   --    (ANCESTOR_PART with RECORD_COMPONENT_ASSOCIATION_LIST)

   --  ANCESTOR_PART ::= EXPRESSION | SUBTYPE_MARK

   --  ARRAY_AGGREGATE ::=
   --    POSITIONAL_ARRAY_AGGREGATE | NAMED_ARRAY_AGGREGATE

   --  POSITIONAL_ARRAY_AGGREGATE ::=
   --    (EXPRESSION, EXPRESSION {, EXPRESSION})
   --  | (EXPRESSION {, EXPRESSION}, others => EXPRESSION)
   --  | (EXPRESSION {, EXPRESSION}, others => <>)

   --  NAMED_ARRAY_AGGREGATE ::=
   --    (ARRAY_COMPONENT_ASSOCIATION {, ARRAY_COMPONENT_ASSOCIATION})

   --  PRIMARY ::= (EXPRESSION);

   --  Error recovery: can raise Error_Resync

   --  Note: POSITIONAL_ARRAY_AGGREGATE rule has been extended to give support
   --        to Ada 2005 limited aggregates (AI-287)

   function P_Aggregate_Or_Paren_Expr return Node_Id is
      Aggregate_Node : Node_Id;
      Expr_List      : List_Id;
      Assoc_List     : List_Id;
      Expr_Node      : Node_Id;
      Lparen_Sloc    : Source_Ptr;
      Scan_State     : Saved_Scan_State;

      procedure Box_Error;
      --  Called if <> is encountered as positional aggregate element. Issues
      --  error message and sets Expr_Node to Error.

      ---------------
      -- Box_Error --
      ---------------

      procedure Box_Error is
      begin
         if Ada_Version < Ada_2005 then
            Error_Msg_SC ("box in aggregate is an Ada 2005 extension");
         end if;

         --  Ada 2005 (AI-287): The box notation is allowed only with named
         --  notation because positional notation might be error prone. For
         --  example, in "(X, <>, Y, <>)", there is no type associated with
         --  the boxes, so you might not be leaving out the components you
         --  thought you were leaving out.

         Error_Msg_SC ("(Ada 2005) box only allowed with named notation");
         Scan; -- past box
         Expr_Node := Error;
      end Box_Error;

   --  Start of processing for P_Aggregate_Or_Paren_Expr

   begin
      Lparen_Sloc := Token_Ptr;
      T_Left_Paren;

      --  Note on parentheses count. For cases like an if expression, the
      --  parens here really count as real parentheses for the paren count,
      --  so we adjust the paren count accordingly after scanning the expr.

      --  If expression

      if Token = Tok_If then
         Expr_Node := P_If_Expression;
         T_Right_Paren;
         Set_Paren_Count (Expr_Node, Paren_Count (Expr_Node) + 1);
         return Expr_Node;

      --  Case expression

      elsif Token = Tok_Case then
         Expr_Node := P_Case_Expression;
         T_Right_Paren;
         Set_Paren_Count (Expr_Node, Paren_Count (Expr_Node) + 1);
         return Expr_Node;

      --  Quantified expression

      elsif Token = Tok_For then
         Expr_Node := P_Quantified_Expression;
         T_Right_Paren;
         Set_Paren_Count (Expr_Node, Paren_Count (Expr_Node) + 1);
         return Expr_Node;

      --  Note: the mechanism used here of rescanning the initial expression
      --  is distinctly unpleasant, but it saves a lot of fiddling in scanning
      --  out the discrete choice list.

      --  Deal with expression and extension aggregates first

      elsif Token /= Tok_Others then
         Save_Scan_State (Scan_State); -- at start of expression

         --  Deal with (NULL RECORD)

         if Token = Tok_Null then
            Scan; -- past NULL

            if Token = Tok_Record then
               Aggregate_Node := New_Node (N_Aggregate, Lparen_Sloc);
               Set_Null_Record_Present (Aggregate_Node, True);
               Scan; -- past RECORD
               T_Right_Paren;
               return Aggregate_Node;
            else
               Restore_Scan_State (Scan_State); -- to NULL that must be expr
            end if;
         end if;

         --  Scan expression, handling box appearing as positional argument

         if Token = Tok_Box then
            Box_Error;
         else
            Expr_Node := P_Expression_Or_Range_Attribute_If_OK;
         end if;

         --  Extension aggregate

         if Token = Tok_With then
            if Nkind (Expr_Node) = N_Attribute_Reference
              and then Attribute_Name (Expr_Node) = Name_Range
            then
               Bad_Range_Attribute (Sloc (Expr_Node));
               return Error;
            end if;

            if Ada_Version = Ada_83 then
               Error_Msg_SC ("(Ada 83) extension aggregate not allowed");
            end if;

            Aggregate_Node := New_Node (N_Extension_Aggregate, Lparen_Sloc);
            Set_Ancestor_Part (Aggregate_Node, Expr_Node);
            Scan; -- past WITH

            --  Deal with WITH NULL RECORD case

            if Token = Tok_Null then
               Save_Scan_State (Scan_State); -- at NULL
               Scan; -- past NULL

               if Token = Tok_Record then
                  Scan; -- past RECORD
                  Set_Null_Record_Present (Aggregate_Node, True);
                  T_Right_Paren;
                  return Aggregate_Node;

               else
                  Restore_Scan_State (Scan_State); -- to NULL that must be expr
               end if;
            end if;

            if Token /= Tok_Others then
               Save_Scan_State (Scan_State);
               Expr_Node := P_Expression;
            else
               Expr_Node := Empty;
            end if;

         --  Expression

         elsif Token = Tok_Right_Paren or else Token in Token_Class_Eterm then
            if Nkind (Expr_Node) = N_Attribute_Reference
              and then Attribute_Name (Expr_Node) = Name_Range
            then
               Error_Msg
                 ("|parentheses not allowed for range attribute", Lparen_Sloc);
               Scan; -- past right paren
               return Expr_Node;
            end if;

            --  Bump paren count of expression

            if Expr_Node /= Error then
               Set_Paren_Count (Expr_Node, Paren_Count (Expr_Node) + 1);
            end if;

            T_Right_Paren; -- past right paren (error message if none)
            return Expr_Node;

         --  Normal aggregate

         else
            Aggregate_Node := New_Node (N_Aggregate, Lparen_Sloc);
         end if;

      --  Others

      else
         Aggregate_Node := New_Node (N_Aggregate, Lparen_Sloc);
         Expr_Node := Empty;
      end if;

      --  Prepare to scan list of component associations

      Expr_List  := No_List; -- don't set yet, maybe all named entries
      Assoc_List := No_List; -- don't set yet, maybe all positional entries

      --  This loop scans through component associations. On entry to the
      --  loop, an expression has been scanned at the start of the current
      --  association unless initial token was OTHERS, in which case
      --  Expr_Node is set to Empty.

      loop
         --  Deal with others association first. This is a named association

         if No (Expr_Node) then
            if No (Assoc_List) then
               Assoc_List := New_List;
            end if;

            Append (P_Record_Or_Array_Component_Association, Assoc_List);

         --  Improper use of WITH

         elsif Token = Tok_With then
            Error_Msg_SC ("WITH must be preceded by single expression in " &
                             "extension aggregate");
            raise Error_Resync;

         --  Range attribute can only appear as part of a discrete choice list

         elsif Nkind (Expr_Node) = N_Attribute_Reference
           and then Attribute_Name (Expr_Node) = Name_Range
           and then Token /= Tok_Arrow
           and then Token /= Tok_Vertical_Bar
         then
            Bad_Range_Attribute (Sloc (Expr_Node));
            return Error;

         --  Assume positional case if comma, right paren, or literal or
         --  identifier or OTHERS follows (the latter cases are missing
         --  comma cases). Also assume positional if a semicolon follows,
         --  which can happen if there are missing parens

         elsif Token = Tok_Comma
           or else Token = Tok_Right_Paren
           or else Token = Tok_Others
           or else Token in Token_Class_Lit_Or_Name
           or else Token = Tok_Semicolon
         then
            if Present (Assoc_List) then
               Error_Msg_BC -- CODEFIX
                  ("""='>"" expected (positional association cannot follow " &
                   "named association)");
            end if;

            if No (Expr_List) then
               Expr_List := New_List;
            end if;

            Append (Expr_Node, Expr_List);

         --  Check for aggregate followed by left parent, maybe missing comma

         elsif Nkind (Expr_Node) = N_Aggregate
           and then Token = Tok_Left_Paren
         then
            T_Comma;

            if No (Expr_List) then
               Expr_List := New_List;
            end if;

            Append (Expr_Node, Expr_List);

         --  Anything else is assumed to be a named association

         else
            Restore_Scan_State (Scan_State); -- to start of expression

            if No (Assoc_List) then
               Assoc_List := New_List;
            end if;

            Append (P_Record_Or_Array_Component_Association, Assoc_List);
         end if;

         exit when not Comma_Present;

         --  If we are at an expression terminator, something is seriously
         --  wrong, so let's get out now, before we start eating up stuff
         --  that doesn't belong to us!

         if Token in Token_Class_Eterm then
            Error_Msg_AP
              ("expecting expression or component association");
            exit;
         end if;

         --  Deal with misused box

         if Token = Tok_Box then
            Box_Error;

         --  Otherwise initiate for reentry to top of loop by scanning an
         --  initial expression, unless the first token is OTHERS.

         elsif Token = Tok_Others then
            Expr_Node := Empty;

         else
            Save_Scan_State (Scan_State); -- at start of expression
            Expr_Node := P_Expression_Or_Range_Attribute_If_OK;

         end if;
      end loop;

      --  All component associations (positional and named) have been scanned

      T_Right_Paren;
      Set_Expressions (Aggregate_Node, Expr_List);
      Set_Component_Associations (Aggregate_Node, Assoc_List);
      return Aggregate_Node;
   end P_Aggregate_Or_Paren_Expr;

   ------------------------------------------------
   -- 4.3  Record or Array Component Association --
   ------------------------------------------------

   --  RECORD_COMPONENT_ASSOCIATION ::=
   --    [COMPONENT_CHOICE_LIST =>] EXPRESSION
   --  | COMPONENT_CHOICE_LIST => <>

   --  COMPONENT_CHOICE_LIST =>
   --    component_SELECTOR_NAME {| component_SELECTOR_NAME}
   --  | others

   --  ARRAY_COMPONENT_ASSOCIATION ::=
   --    DISCRETE_CHOICE_LIST => EXPRESSION
   --  | DISCRETE_CHOICE_LIST => <>

   --  Note: this routine only handles the named cases, including others.
   --  Cases where the component choice list is not present have already
   --  been handled directly.

   --  Error recovery: can raise Error_Resync

   --  Note: RECORD_COMPONENT_ASSOCIATION and ARRAY_COMPONENT_ASSOCIATION
   --        rules have been extended to give support to Ada 2005 limited
   --        aggregates (AI-287)

   function P_Record_Or_Array_Component_Association return Node_Id is
      Assoc_Node : Node_Id;

   begin
      Assoc_Node := New_Node (N_Component_Association, Token_Ptr);
      Set_Choices (Assoc_Node, P_Discrete_Choice_List);
      Set_Sloc (Assoc_Node, Token_Ptr);
      TF_Arrow;

      if Token = Tok_Box then

         --  Ada 2005(AI-287): The box notation is used to indicate the
         --  default initialization of aggregate components

         if Ada_Version < Ada_2005 then
            Error_Msg_SP
              ("component association with '<'> is an Ada 2005 extension");
            Error_Msg_SP ("\unit must be compiled with -gnat05 switch");
         end if;

         Set_Box_Present (Assoc_Node);
         Scan; -- Past box
      else
         Set_Expression (Assoc_Node, P_Expression);
      end if;

      return Assoc_Node;
   end P_Record_Or_Array_Component_Association;

   -----------------------------
   -- 4.3.1  Record Aggregate --
   -----------------------------

   --  Case of enumeration aggregate is parsed by P_Aggregate (4.3)
   --  All other cases are parsed by P_Aggregate_Or_Paren_Expr (4.3)

   ----------------------------------------------
   -- 4.3.1  Record Component Association List --
   ----------------------------------------------

   --  Parsed by P_Aggregate_Or_Paren_Expr (4.3)

   ----------------------------------
   -- 4.3.1  Component Choice List --
   ----------------------------------

   --  Parsed by P_Aggregate_Or_Paren_Expr (4.3)

   --------------------------------
   -- 4.3.1  Extension Aggregate --
   --------------------------------

   --  Parsed by P_Aggregate_Or_Paren_Expr (4.3)

   --------------------------
   -- 4.3.1  Ancestor Part --
   --------------------------

   --  Parsed by P_Aggregate_Or_Paren_Expr (4.3)

   ----------------------------
   -- 4.3.1  Array Aggregate --
   ----------------------------

   --  Parsed by P_Aggregate_Or_Paren_Expr (4.3)

   ---------------------------------------
   -- 4.3.1  Positional Array Aggregate --
   ---------------------------------------

   --  Parsed by P_Aggregate_Or_Paren_Expr (4.3)

   ----------------------------------
   -- 4.3.1  Named Array Aggregate --
   ----------------------------------

   --  Parsed by P_Aggregate_Or_Paren_Expr (4.3)

   ----------------------------------------
   -- 4.3.1  Array Component Association --
   ----------------------------------------

   --  Parsed by P_Aggregate_Or_Paren_Expr (4.3)

   ---------------------
   -- 4.4  Expression --
   ---------------------

   --  This procedure parses EXPRESSION or CHOICE_EXPRESSION

   --  EXPRESSION ::=
   --    RELATION {LOGICAL_OPERATOR RELATION}

   --  CHOICE_EXPRESSION ::=
   --    CHOICE_RELATION {LOGICAL_OPERATOR CHOICE_RELATION}

   --  LOGICAL_OPERATOR ::= and | and then | or | or else | xor

   --  On return, Expr_Form indicates the categorization of the expression
   --  EF_Range_Attr is not a possible value (if a range attribute is found,
   --  an error message is given, and Error is returned).

   --  Error recovery: cannot raise Error_Resync

   function P_Expression return Node_Id is
      Logical_Op      : Node_Kind;
      Prev_Logical_Op : Node_Kind;
      Op_Location     : Source_Ptr;
      Node1           : Node_Id;
      Node2           : Node_Id;

   begin
      Node1 := P_Relation;

      if Token in Token_Class_Logop then
         Prev_Logical_Op := N_Empty;

         loop
            Op_Location := Token_Ptr;
            Logical_Op := P_Logical_Operator;

            if Prev_Logical_Op /= N_Empty and then
               Logical_Op /= Prev_Logical_Op
            then
               Error_Msg
                 ("mixed logical operators in expression", Op_Location);
               Prev_Logical_Op := N_Empty;
            else
               Prev_Logical_Op := Logical_Op;
            end if;

            Node2 := Node1;
            Node1 := New_Op_Node (Logical_Op, Op_Location);
            Set_Left_Opnd (Node1, Node2);
            Set_Right_Opnd (Node1, P_Relation);
            exit when Token not in Token_Class_Logop;
         end loop;

         Expr_Form := EF_Non_Simple;
      end if;

      if Token = Tok_Apostrophe then
         Bad_Range_Attribute (Token_Ptr);
         return Error;
      else
         return Node1;
      end if;
   end P_Expression;

   --  This function is identical to the normal P_Expression, except that it
   --  also permits the appearance of a case, conditional, or quantified
   --  expression if the call immediately follows a left paren, and followed
   --  by a right parenthesis. These forms are allowed if these conditions
   --  are not met, but an error message will be issued.

   function P_Expression_If_OK return Node_Id is
   begin
      --  Case of conditional, case or quantified expression

      if Token = Tok_Case or else Token = Tok_If or else Token = Tok_For then
         return P_Unparen_Cond_Case_Quant_Expression;

      --  Normal case, not case/conditional/quantified expression

      else
         return P_Expression;
      end if;
   end P_Expression_If_OK;

   --  This function is identical to the normal P_Expression, except that it
   --  checks that the expression scan did not stop on a right paren. It is
   --  called in all contexts where a right parenthesis cannot legitimately
   --  follow an expression.

   --  Error recovery: can not raise Error_Resync

   function P_Expression_No_Right_Paren return Node_Id is
      Expr : constant Node_Id := P_Expression;
   begin
      Ignore (Tok_Right_Paren);
      return Expr;
   end P_Expression_No_Right_Paren;

   ----------------------------------------
   -- 4.4  Expression_Or_Range_Attribute --
   ----------------------------------------

   --  EXPRESSION ::=
   --    RELATION {and RELATION} | RELATION {and then RELATION}
   --  | RELATION {or RELATION}  | RELATION {or else RELATION}
   --  | RELATION {xor RELATION}

   --  RANGE_ATTRIBUTE_REFERENCE ::= PREFIX ' RANGE_ATTRIBUTE_DESIGNATOR

   --  RANGE_ATTRIBUTE_DESIGNATOR ::= range [(static_EXPRESSION)]

   --  On return, Expr_Form indicates the categorization of the expression
   --  and EF_Range_Attr is one of the possibilities.

   --  Error recovery: cannot raise Error_Resync

   --  In the grammar, a RANGE attribute is simply a name, but its use is
   --  highly restricted, so in the parser, we do not regard it as a name.
   --  Instead, P_Name returns without scanning the 'RANGE part of the
   --  attribute, and P_Expression_Or_Range_Attribute handles the range
   --  attribute reference. In the normal case where a range attribute is
   --  not allowed, an error message is issued by P_Expression.

   function P_Expression_Or_Range_Attribute return Node_Id is
      Logical_Op      : Node_Kind;
      Prev_Logical_Op : Node_Kind;
      Op_Location     : Source_Ptr;
      Node1           : Node_Id;
      Node2           : Node_Id;
      Attr_Node       : Node_Id;

   begin
      Node1 := P_Relation;

      if Token = Tok_Apostrophe then
         Attr_Node := P_Range_Attribute_Reference (Node1);
         Expr_Form := EF_Range_Attr;
         return Attr_Node;

      elsif Token in Token_Class_Logop then
         Prev_Logical_Op := N_Empty;

         loop
            Op_Location := Token_Ptr;
            Logical_Op := P_Logical_Operator;

            if Prev_Logical_Op /= N_Empty and then
               Logical_Op /= Prev_Logical_Op
            then
               Error_Msg
                 ("mixed logical operators in expression", Op_Location);
               Prev_Logical_Op := N_Empty;
            else
               Prev_Logical_Op := Logical_Op;
            end if;

            Node2 := Node1;
            Node1 := New_Op_Node (Logical_Op, Op_Location);
            Set_Left_Opnd (Node1, Node2);
            Set_Right_Opnd (Node1, P_Relation);
            exit when Token not in Token_Class_Logop;
         end loop;

         Expr_Form := EF_Non_Simple;
      end if;

      if Token = Tok_Apostrophe then
         Bad_Range_Attribute (Token_Ptr);
         return Error;
      else
         return Node1;
      end if;
   end P_Expression_Or_Range_Attribute;

   --  Version that allows a non-parenthesized case, conditional, or quantified
   --  expression if the call immediately follows a left paren, and followed
   --  by a right parenthesis. These forms are allowed if these conditions
   --  are not met, but an error message will be issued.

   function P_Expression_Or_Range_Attribute_If_OK return Node_Id is
   begin
      --  Case of conditional, case or quantified expression

      if Token = Tok_Case or else Token = Tok_If or else Token = Tok_For then
         return P_Unparen_Cond_Case_Quant_Expression;

      --  Normal case, not one of the above expression types

      else
         return P_Expression_Or_Range_Attribute;
      end if;
   end P_Expression_Or_Range_Attribute_If_OK;

   -------------------
   -- 4.4  Relation --
   -------------------

   --  This procedure scans both relations and choice relations

   --  CHOICE_RELATION ::=
   --    SIMPLE_EXPRESSION [RELATIONAL_OPERATOR SIMPLE_EXPRESSION]

   --  RELATION ::=
   --    SIMPLE_EXPRESSION [not] in MEMBERSHIP_CHOICE_LIST
   --  | RAISE_EXPRESSION

   --  MEMBERSHIP_CHOICE_LIST ::=
   --    MEMBERSHIP_CHOICE {'|' MEMBERSHIP CHOICE}

   --  MEMBERSHIP_CHOICE ::=
   --    CHOICE_EXPRESSION | RANGE | SUBTYPE_MARK

   --  RAISE_EXPRESSION ::= raise exception_NAME [with string_EXPRESSION]

   --  On return, Expr_Form indicates the categorization of the expression

   --  Note: if Token = Tok_Apostrophe on return, then Expr_Form is set to
   --  EF_Simple_Name and the following token is RANGE (range attribute case).

   --  Error recovery: cannot raise Error_Resync. If an error occurs within an
   --  expression, then tokens are scanned until either a non-expression token,
   --  a right paren (not matched by a left paren) or a comma, is encountered.

   function P_Relation return Node_Id is
      Node1, Node2 : Node_Id;
      Optok        : Source_Ptr;

   begin
      --  First check for raise expression

      if Token = Tok_Raise then
         Expr_Form := EF_Non_Simple;
         return P_Raise_Expression;
      end if;

      --  All other cases

      Node1 := P_Simple_Expression;

      if Token not in Token_Class_Relop then
         return Node1;

      else
         --  Here we have a relational operator following. If so then scan it
         --  out. Note that the assignment symbol := is treated as a relational
         --  operator to improve the error recovery when it is misused for =.
         --  P_Relational_Operator also parses the IN and NOT IN operations.

         Optok := Token_Ptr;
         Node2 := New_Op_Node (P_Relational_Operator, Optok);
         Set_Left_Opnd (Node2, Node1);

         --  Case of IN or NOT IN

         if Prev_Token = Tok_In then
            P_Membership_Test (Node2);

         --  Case of relational operator (= /= < <= > >=)

         else
            Set_Right_Opnd (Node2, P_Simple_Expression);
         end if;

         Expr_Form := EF_Non_Simple;

         if Token in Token_Class_Relop then
            Error_Msg_SC ("unexpected relational operator");
            raise Error_Resync;
         end if;

         return Node2;
      end if;

   --  If any error occurs, then scan to the next expression terminator symbol
   --  or comma or right paren at the outer (i.e. current) parentheses level.
   --  The flags are set to indicate a normal simple expression.

   exception
      when Error_Resync =>
         Resync_Expression;
         Expr_Form := EF_Simple;
         return Error;
   end P_Relation;

   ----------------------------
   -- 4.4  Simple Expression --
   ----------------------------

   --  SIMPLE_EXPRESSION ::=
   --    [UNARY_ADDING_OPERATOR] TERM {BINARY_ADDING_OPERATOR TERM}

   --  On return, Expr_Form indicates the categorization of the expression

   --  Note: if Token = Tok_Apostrophe on return, then Expr_Form is set to
   --  EF_Simple_Name and the following token is RANGE (range attribute case).

   --  Error recovery: cannot raise Error_Resync. If an error occurs within an
   --  expression, then tokens are scanned until either a non-expression token,
   --  a right paren (not matched by a left paren) or a comma, is encountered.

   --  Note: P_Simple_Expression is called only internally by higher level
   --  expression routines. In cases in the grammar where a simple expression
   --  is required, the approach is to scan an expression, and then post an
   --  appropriate error message if the expression obtained is not simple. This
   --  gives better error recovery and treatment.

   function P_Simple_Expression return Node_Id is
      Scan_State : Saved_Scan_State;
      Node1      : Node_Id;
      Node2      : Node_Id;
      Tokptr     : Source_Ptr;

   begin
      --  Check for cases starting with a name. There are two reasons for
      --  special casing. First speed things up by catching a common case
      --  without going through several routine layers. Second the caller must
      --  be informed via Expr_Form when the simple expression is a name.

      if Token in Token_Class_Name then
         Node1 := P_Name;

         --  Deal with apostrophe cases

         if Token = Tok_Apostrophe then
            Save_Scan_State (Scan_State); -- at apostrophe
            Scan; -- past apostrophe

            --  If qualified expression, scan it out and fall through

            if Token = Tok_Left_Paren then
               Node1 := P_Qualified_Expression (Node1);
               Expr_Form := EF_Simple;

            --  If range attribute, then we return with Token pointing to the
            --  apostrophe. Note: avoid the normal error check on exit. We
            --  know that the expression really is complete in this case!

            else -- Token = Tok_Range then
               Restore_Scan_State (Scan_State); -- to apostrophe
               Expr_Form := EF_Simple_Name;
               return Node1;
            end if;
         end if;

         --  If an expression terminator follows, the previous processing
         --  completely scanned out the expression (a common case), and
         --  left Expr_Form set appropriately for returning to our caller.

         if Token in Token_Class_Sterm then
            null;

         --  If we do not have an expression terminator, then complete the
         --  scan of a simple expression. This code duplicates the code
         --  found in P_Term and P_Factor.

         else
            if Token = Tok_Double_Asterisk then
               if Style_Check then
                  Style.Check_Exponentiation_Operator;
               end if;

               Node2 := New_Op_Node (N_Op_Expon, Token_Ptr);
               Scan; -- past **
               Set_Left_Opnd (Node2, Node1);
               Set_Right_Opnd (Node2, P_Primary);
               Check_Bad_Exp;
               Node1 := Node2;
            end if;

            loop
               exit when Token not in Token_Class_Mulop;
               Tokptr := Token_Ptr;
               Node2 := New_Op_Node (P_Multiplying_Operator, Tokptr);

               if Style_Check then
                  Style.Check_Binary_Operator;
               end if;

               Scan; -- past operator
               Set_Left_Opnd (Node2, Node1);
               Set_Right_Opnd (Node2, P_Factor);
               Node1 := Node2;
            end loop;

            loop
               exit when Token not in Token_Class_Binary_Addop;
               Tokptr := Token_Ptr;
               Node2 := New_Op_Node (P_Binary_Adding_Operator, Tokptr);

               if Style_Check then
                  Style.Check_Binary_Operator;
               end if;

               Scan; -- past operator
               Set_Left_Opnd (Node2, Node1);
               Set_Right_Opnd (Node2, P_Term);
               Node1 := Node2;
            end loop;

            Expr_Form := EF_Simple;
         end if;

      --  Cases where simple expression does not start with a name

      else
         --  Scan initial sign and initial Term

         if Token in Token_Class_Unary_Addop then
            Tokptr := Token_Ptr;
            Node1 := New_Op_Node (P_Unary_Adding_Operator, Tokptr);

            if Style_Check then
               Style.Check_Unary_Plus_Or_Minus;
            end if;

            Scan; -- past operator
            Set_Right_Opnd (Node1, P_Term);
         else
            Node1 := P_Term;
         end if;

         --  In the following, we special-case a sequence of concatenations of
         --  string literals, such as "aaa" & "bbb" & ... & "ccc", with nothing
         --  else mixed in. For such a sequence, we return a tree representing
         --  "" & "aaabbb...ccc" (a single concatenation). This is done only if
         --  the number of concatenations is large. If semantic analysis
         --  resolves the "&" to a predefined one, then this folding gives the
         --  right answer. Otherwise, semantic analysis will complain about a
         --  capacity-exceeded error. The purpose of this trick is to avoid
         --  creating a deeply nested tree, which would cause deep recursion
         --  during semantics, causing stack overflow. This way, we can handle
         --  enormous concatenations in the normal case of predefined "&".  We
         --  first build up the normal tree, and then rewrite it if
         --  appropriate.

         declare
            Num_Concats_Threshold : constant Positive := 1000;
            --  Arbitrary threshold value to enable optimization

            First_Node : constant Node_Id := Node1;
            Is_Strlit_Concat : Boolean;
            --  True iff we've parsed a sequence of concatenations of string
            --  literals, with nothing else mixed in.

            Num_Concats : Natural;
            --  Number of "&" operators if Is_Strlit_Concat is True

         begin
            Is_Strlit_Concat :=
              Nkind (Node1) = N_String_Literal
                and then Token = Tok_Ampersand;
            Num_Concats := 0;

            --  Scan out sequence of terms separated by binary adding operators

            loop
               exit when Token not in Token_Class_Binary_Addop;
               Tokptr := Token_Ptr;
               Node2 := New_Op_Node (P_Binary_Adding_Operator, Tokptr);
               Scan; -- past operator
               Set_Left_Opnd (Node2, Node1);
               Node1 := P_Term;
               Set_Right_Opnd (Node2, Node1);

               --  Check if we're still concatenating string literals

               Is_Strlit_Concat :=
                 Is_Strlit_Concat
                   and then Nkind (Node2) = N_Op_Concat
                 and then Nkind (Node1) = N_String_Literal;

               if Is_Strlit_Concat then
                  Num_Concats := Num_Concats + 1;
               end if;

               Node1 := Node2;
            end loop;

            --  If we have an enormous series of concatenations of string
            --  literals, rewrite as explained above. The Is_Folded_In_Parser
            --  flag tells semantic analysis that if the "&" is not predefined,
            --  the folded value is wrong.

            if Is_Strlit_Concat
              and then Num_Concats >= Num_Concats_Threshold
            then
               declare
                  Empty_String_Val : String_Id;
                  --  String_Id for ""

                  Strlit_Concat_Val : String_Id;
                  --  Contains the folded value (which will be correct if the
                  --  "&" operators are the predefined ones).

                  Cur_Node : Node_Id;
                  --  For walking up the tree

                  New_Node : Node_Id;
                  --  Folded node to replace Node1

                  Loc : constant Source_Ptr := Sloc (First_Node);

               begin
                  --  Walk up the tree starting at the leftmost string literal
                  --  (First_Node), building up the Strlit_Concat_Val as we
                  --  go. Note that we do not use recursion here -- the whole
                  --  point is to avoid recursively walking that enormous tree.

                  Start_String;
                  Store_String_Chars (Strval (First_Node));

                  Cur_Node := Parent (First_Node);
                  while Present (Cur_Node) loop
                     pragma Assert (Nkind (Cur_Node) = N_Op_Concat and then
                        Nkind (Right_Opnd (Cur_Node)) = N_String_Literal);

                     Store_String_Chars (Strval (Right_Opnd (Cur_Node)));
                     Cur_Node := Parent (Cur_Node);
                  end loop;

                  Strlit_Concat_Val := End_String;

                  --  Create new folded node, and rewrite result with a concat-
                  --  enation of an empty string literal and the folded node.

                  Start_String;
                  Empty_String_Val := End_String;
                  New_Node :=
                    Make_Op_Concat (Loc,
                      Make_String_Literal (Loc, Empty_String_Val),
                      Make_String_Literal (Loc, Strlit_Concat_Val,
                        Is_Folded_In_Parser => True));
                  Rewrite (Node1, New_Node);
               end;
            end if;
         end;

         --  All done, we clearly do not have name or numeric literal so this
         --  is a case of a simple expression which is some other possibility.

         Expr_Form := EF_Simple;
      end if;

      --  Come here at end of simple expression, where we do a couple of
      --  special checks to improve error recovery.

      --  Special test to improve error recovery. If the current token
      --  is a period, then someone is trying to do selection on something
      --  that is not a name, e.g. a qualified expression.

      if Token = Tok_Dot then
         Error_Msg_SC ("prefix for selection is not a name");

         --  If qualified expression, comment and continue, otherwise something
         --  is pretty nasty so do an Error_Resync call.

         if Ada_Version < Ada_2012
           and then Nkind (Node1) = N_Qualified_Expression
         then
            Error_Msg_SC ("\would be legal in Ada 2012 mode");
         else
            raise Error_Resync;
         end if;
      end if;

      --  Special test to improve error recovery: If the current token is
      --  not the first token on a line (as determined by checking the
      --  previous token position with the start of the current line),
      --  then we insist that we have an appropriate terminating token.
      --  Consider the following two examples:

      --   1)  if A nad B then ...

      --   2)  A := B
      --       C := D

      --  In the first example, we would like to issue a binary operator
      --  expected message and resynchronize to the then. In the second
      --  example, we do not want to issue a binary operator message, so
      --  that instead we will get the missing semicolon message. This
      --  distinction is of course a heuristic which does not always work,
      --  but in practice it is quite effective.

      --  Note: the one case in which we do not go through this circuit is
      --  when we have scanned a range attribute and want to return with
      --  Token pointing to the apostrophe. The apostrophe is not normally
      --  an expression terminator, and is not in Token_Class_Sterm, but
      --  in this special case we know that the expression is complete.

      if not Token_Is_At_Start_Of_Line
         and then Token not in Token_Class_Sterm
      then
         --  Normally the right error message is indeed that we expected a
         --  binary operator, but in the case of being between a right and left
         --  paren, e.g. in an aggregate, a more likely error is missing comma.

         if Prev_Token = Tok_Right_Paren and then Token = Tok_Left_Paren then
            T_Comma;
         else
            Error_Msg_AP ("binary operator expected");
         end if;

         raise Error_Resync;

      else
         return Node1;
      end if;

   --  If any error occurs, then scan to next expression terminator symbol
   --  or comma, right paren or vertical bar at the outer (i.e. current) paren
   --  level. Expr_Form is set to indicate a normal simple expression.

   exception
      when Error_Resync =>
         Resync_Expression;
         Expr_Form := EF_Simple;
         return Error;
   end P_Simple_Expression;

   -----------------------------------------------
   -- 4.4  Simple Expression or Range Attribute --
   -----------------------------------------------

   --  SIMPLE_EXPRESSION ::=
   --    [UNARY_ADDING_OPERATOR] TERM {BINARY_ADDING_OPERATOR TERM}

   --  RANGE_ATTRIBUTE_REFERENCE ::= PREFIX ' RANGE_ATTRIBUTE_DESIGNATOR

   --  RANGE_ATTRIBUTE_DESIGNATOR ::= range [(static_EXPRESSION)]

   --  Error recovery: cannot raise Error_Resync

   function P_Simple_Expression_Or_Range_Attribute return Node_Id is
      Sexpr     : Node_Id;
      Attr_Node : Node_Id;

   begin
      --  We don't just want to roar ahead and call P_Simple_Expression
      --  here, since we want to handle the case of a parenthesized range
      --  attribute cleanly.

      if Token = Tok_Left_Paren then
         declare
            Lptr       : constant Source_Ptr := Token_Ptr;
            Scan_State : Saved_Scan_State;

         begin
            Save_Scan_State (Scan_State);
            Scan; -- past left paren
            Sexpr := P_Simple_Expression;

            if Token = Tok_Apostrophe then
               Attr_Node := P_Range_Attribute_Reference (Sexpr);
               Expr_Form := EF_Range_Attr;

               if Token = Tok_Right_Paren then
                  Scan; -- scan past right paren if present
               end if;

               Error_Msg ("parentheses not allowed for range attribute", Lptr);

               return Attr_Node;
            end if;

            Restore_Scan_State (Scan_State);
         end;
      end if;

      --  Here after dealing with parenthesized range attribute

      Sexpr := P_Simple_Expression;

      if Token = Tok_Apostrophe then
         Attr_Node := P_Range_Attribute_Reference (Sexpr);
         Expr_Form := EF_Range_Attr;
         return Attr_Node;

      else
         return Sexpr;
      end if;
   end P_Simple_Expression_Or_Range_Attribute;

   ---------------
   -- 4.4  Term --
   ---------------

   --  TERM ::= FACTOR {MULTIPLYING_OPERATOR FACTOR}

   --  Error recovery: can raise Error_Resync

   function P_Term return Node_Id is
      Node1, Node2 : Node_Id;
      Tokptr       : Source_Ptr;

   begin
      Node1 := P_Factor;

      loop
         exit when Token not in Token_Class_Mulop;
         Tokptr := Token_Ptr;
         Node2 := New_Op_Node (P_Multiplying_Operator, Tokptr);
         Scan; -- past operator
         Set_Left_Opnd (Node2, Node1);
         Set_Right_Opnd (Node2, P_Factor);
         Node1 := Node2;
      end loop;

      return Node1;
   end P_Term;

   -----------------
   -- 4.4  Factor --
   -----------------

   --  FACTOR ::= PRIMARY [** PRIMARY] | abs PRIMARY | not PRIMARY

   --  Error recovery: can raise Error_Resync

   function P_Factor return Node_Id is
      Node1 : Node_Id;
      Node2 : Node_Id;

   begin
      if Token = Tok_Abs then
         Node1 := New_Op_Node (N_Op_Abs, Token_Ptr);

         if Style_Check then
            Style.Check_Abs_Not;
         end if;

         Scan; -- past ABS
         Set_Right_Opnd (Node1, P_Primary);
         return Node1;

      elsif Token = Tok_Not then
         Node1 := New_Op_Node (N_Op_Not, Token_Ptr);

         if Style_Check then
            Style.Check_Abs_Not;
         end if;

         Scan; -- past NOT
         Set_Right_Opnd (Node1, P_Primary);
         return Node1;

      else
         Node1 := P_Primary;

         if Token = Tok_Double_Asterisk then
            Node2 := New_Op_Node (N_Op_Expon, Token_Ptr);
            Scan; -- past **
            Set_Left_Opnd (Node2, Node1);
            Set_Right_Opnd (Node2, P_Primary);
            Check_Bad_Exp;
            return Node2;
         else
            return Node1;
         end if;
      end if;
   end P_Factor;

   ------------------
   -- 4.4  Primary --
   ------------------

   --  PRIMARY ::=
   --    NUMERIC_LITERAL  | null
   --  | STRING_LITERAL   | AGGREGATE
   --  | NAME             | QUALIFIED_EXPRESSION
   --  | ALLOCATOR        | (EXPRESSION) | QUANTIFIED_EXPRESSION

   --  Error recovery: can raise Error_Resync

   function P_Primary return Node_Id is
      Scan_State : Saved_Scan_State;
      Node1      : Node_Id;

      Lparen : constant Boolean := Prev_Token = Tok_Left_Paren;
      --  Remember if previous token is a left parenthesis. This is used to
      --  deal with checking whether IF/CASE/FOR expressions appearing as
      --  primaries require extra parenthesization.

   begin
      --  The loop runs more than once only if misplaced pragmas are found
      --  or if a misplaced unary minus is skipped.

      loop
         case Token is

            --  Name token can start a name, call or qualified expression, all
            --  of which are acceptable possibilities for primary. Note also
            --  that string literal is included in name (as operator symbol)
            --  and type conversion is included in name (as indexed component).

            when Tok_Char_Literal | Tok_Operator_Symbol | Tok_Identifier =>
               Node1 := P_Name;

               --  All done unless apostrophe follows

               if Token /= Tok_Apostrophe then
                  return Node1;

               --  Apostrophe following means that we have either just parsed
               --  the subtype mark of a qualified expression, or the prefix
               --  or a range attribute.

               else -- Token = Tok_Apostrophe
                  Save_Scan_State (Scan_State); -- at apostrophe
                  Scan; -- past apostrophe

                  --  If range attribute, then this is always an error, since
                  --  the only legitimate case (where the scanned expression is
                  --  a qualified simple name) is handled at the level of the
                  --  Simple_Expression processing. This case corresponds to a
                  --  usage such as 3 + A'Range, which is always illegal.

                  if Token = Tok_Range then
                     Restore_Scan_State (Scan_State); -- to apostrophe
                     Bad_Range_Attribute (Token_Ptr);
                     return Error;

                  --  If left paren, then we have a qualified expression.
                  --  Note that P_Name guarantees that in this case, where
                  --  Token = Tok_Apostrophe on return, the only two possible
                  --  tokens following the apostrophe are left paren and
                  --  RANGE, so we know we have a left paren here.

                  else -- Token = Tok_Left_Paren
                     return P_Qualified_Expression (Node1);

                  end if;
               end if;

            --  Numeric or string literal

            when Tok_Integer_Literal |
                 Tok_Real_Literal    |
                 Tok_String_Literal  =>

               Node1 := Token_Node;
               Scan; -- past number
               return Node1;

            --  Left paren, starts aggregate or parenthesized expression

            when Tok_Left_Paren =>
               declare
                  Expr : constant Node_Id := P_Aggregate_Or_Paren_Expr;

               begin
                  if Nkind (Expr) = N_Attribute_Reference
                    and then Attribute_Name (Expr) = Name_Range
                  then
                     Bad_Range_Attribute (Sloc (Expr));
                  end if;

                  return Expr;
               end;

            --  Allocator

            when Tok_New =>
               return P_Allocator;

            --  Null

            when Tok_Null =>
               Scan; -- past NULL
               return New_Node (N_Null, Prev_Token_Ptr);

            --  Pragma, not allowed here, so just skip past it

            when Tok_Pragma =>
               P_Pragmas_Misplaced;

            --  Deal with IF (possible unparenthesized if expression)

            when Tok_If =>

               --  If this looks like a real if, defined as an IF appearing at
               --  the start of a new line, then we consider we have a missing
               --  operand. If in Ada 2012 and the IF is not properly indented
               --  for a statement, we prefer to issue a message about an ill-
               --  parenthesized if expression.

               if Token_Is_At_Start_Of_Line
                 and then not
                   (Ada_Version >= Ada_2012
                     and then Style_Check_Indentation /= 0
                     and then Start_Column rem Style_Check_Indentation /= 0)
               then
                  Error_Msg_AP ("missing operand");
                  return Error;

               --  If this looks like an if expression, then treat it that way
               --  with an error message if not explicitly surrounded by
               --  parentheses.

               elsif Ada_Version >= Ada_2012 then
                  Node1 := P_If_Expression;

                  if not (Lparen and then Token = Tok_Right_Paren) then
                     Error_Msg
                       ("if expression must be parenthesized", Sloc (Node1));
                  end if;

                  return Node1;

               --  Otherwise treat as misused identifier

               else
                  return P_Identifier;
               end if;

            --  Deal with CASE (possible unparenthesized case expression)

            when Tok_Case =>

               --  If this looks like a real case, defined as a CASE appearing
               --  the start of a new line, then we consider we have a missing
               --  operand. If in Ada 2012 and the CASE is not properly
               --  indented for a statement, we prefer to issue a message about
               --  an ill-parenthesized case expression.

               if Token_Is_At_Start_Of_Line
                 and then not
                   (Ada_Version >= Ada_2012
                     and then Style_Check_Indentation /= 0
                     and then Start_Column rem Style_Check_Indentation /= 0)
               then
                  Error_Msg_AP ("missing operand");
                  return Error;

               --  If this looks like a case expression, then treat it that way
               --  with an error message if not within parentheses.

               elsif Ada_Version >= Ada_2012 then
                  Node1 := P_Case_Expression;

                  if not (Lparen and then Token = Tok_Right_Paren) then
                     Error_Msg
                       ("case expression must be parenthesized", Sloc (Node1));
                  end if;

                  return Node1;

               --  Otherwise treat as misused identifier

               else
                  return P_Identifier;
               end if;

            --  For [all | some]  indicates a quantified expression

            when Tok_For =>
               if Token_Is_At_Start_Of_Line then
                  Error_Msg_AP ("misplaced loop");
                  return Error;

               elsif Ada_Version >= Ada_2012 then
                  Node1 := P_Quantified_Expression;

                  if not (Lparen and then Token = Tok_Right_Paren) then
                     Error_Msg
                      ("quantified expression must be parenthesized",
                        Sloc (Node1));
                  end if;

                  return Node1;

               --  Otherwise treat as misused identifier

               else
                  return P_Identifier;
               end if;

            --  Minus may well be an improper attempt at a unary minus. Give
            --  a message, skip the minus and keep going!

            when Tok_Minus =>
               Error_Msg_SC ("parentheses required for unary minus");
               Scan; -- past minus

            --  Anything else is illegal as the first token of a primary, but
            --  we test for some common errors, to improve error messages.

            when others =>
               if Is_Reserved_Identifier then
                  return P_Identifier;

               elsif Prev_Token = Tok_Comma then
                  Error_Msg_SP -- CODEFIX
                    ("|extra "","" ignored");
                  raise Error_Resync;

               else
                  Error_Msg_AP ("missing operand");
                  raise Error_Resync;
               end if;

         end case;
      end loop;
   end P_Primary;

   -------------------------------
   -- 4.4 Quantified_Expression --
   -------------------------------

   --  QUANTIFIED_EXPRESSION ::=
   --    for QUANTIFIER LOOP_PARAMETER_SPECIFICATION => PREDICATE |
   --    for QUANTIFIER ITERATOR_SPECIFICATION => PREDICATE

   function P_Quantified_Expression return Node_Id is
      I_Spec : Node_Id;
      Node1  : Node_Id;

   begin
      if Ada_Version < Ada_2012 then
         Error_Msg_SC ("quantified expression is an Ada 2012 feature");
         Error_Msg_SC ("\|unit must be compiled with -gnat2012 switch");
      end if;

      Scan;  --  past FOR

      Node1 := New_Node (N_Quantified_Expression, Prev_Token_Ptr);

      if Token = Tok_All then
         Set_All_Present (Node1);

      elsif Token /= Tok_Some then
         Error_Msg_AP ("missing quantifier");
         raise Error_Resync;
      end if;

      Scan; -- past SOME
      I_Spec := P_Loop_Parameter_Specification;

      if Nkind (I_Spec) = N_Loop_Parameter_Specification then
         Set_Loop_Parameter_Specification (Node1, I_Spec);
      else
         Set_Iterator_Specification (Node1, I_Spec);
      end if;

      if Token = Tok_Arrow then
         Scan;
         Set_Condition (Node1, P_Expression);
         return Node1;
      else
         Error_Msg_AP ("missing arrow");
         raise Error_Resync;
      end if;
   end P_Quantified_Expression;

   ---------------------------
   -- 4.5  Logical Operator --
   ---------------------------

   --  LOGICAL_OPERATOR  ::=  and | or | xor

   --  Note: AND THEN and OR ELSE are also treated as logical operators
   --  by the parser (even though they are not operators semantically)

   --  The value returned is the appropriate Node_Kind code for the operator
   --  On return, Token points to the token following the scanned operator.

   --  The caller has checked that the first token is a legitimate logical
   --  operator token (i.e. is either XOR, AND, OR).

   --  Error recovery: cannot raise Error_Resync

   function P_Logical_Operator return Node_Kind is
   begin
      if Token = Tok_And then
         if Style_Check then
            Style.Check_Binary_Operator;
         end if;

         Scan; -- past AND

         if Token = Tok_Then then
            Scan; -- past THEN
            return N_And_Then;
         else
            return N_Op_And;
         end if;

      elsif Token = Tok_Or then
         if Style_Check then
            Style.Check_Binary_Operator;
         end if;

         Scan; -- past OR

         if Token = Tok_Else then
            Scan; -- past ELSE
            return N_Or_Else;
         else
            return N_Op_Or;
         end if;

      else -- Token = Tok_Xor
         if Style_Check then
            Style.Check_Binary_Operator;
         end if;

         Scan; -- past XOR
         return N_Op_Xor;
      end if;
   end P_Logical_Operator;

   ------------------------------
   -- 4.5  Relational Operator --
   ------------------------------

   --  RELATIONAL_OPERATOR ::= = | /= | < | <= | > | >=

   --  The value returned is the appropriate Node_Kind code for the operator.
   --  On return, Token points to the operator token, NOT past it.

   --  The caller has checked that the first token is a legitimate relational
   --  operator token (i.e. is one of the operator tokens listed above).

   --  Error recovery: cannot raise Error_Resync

   function P_Relational_Operator return Node_Kind is
      Op_Kind : Node_Kind;
      Relop_Node : constant array (Token_Class_Relop) of Node_Kind :=
                     (Tok_Less          => N_Op_Lt,
                      Tok_Equal         => N_Op_Eq,
                      Tok_Greater       => N_Op_Gt,
                      Tok_Not_Equal     => N_Op_Ne,
                      Tok_Greater_Equal => N_Op_Ge,
                      Tok_Less_Equal    => N_Op_Le,
                      Tok_In            => N_In,
                      Tok_Not           => N_Not_In,
                      Tok_Box           => N_Op_Ne);

   begin
      if Token = Tok_Box then
         Error_Msg_SC -- CODEFIX
           ("|""'<'>"" should be ""/=""");
      end if;

      Op_Kind := Relop_Node (Token);

      if Style_Check then
         Style.Check_Binary_Operator;
      end if;

      Scan; -- past operator token

      --  Deal with NOT IN, if previous token was NOT, we must have IN now

      if Prev_Token = Tok_Not then

         --  Style check, for NOT IN, we require one space between NOT and IN

         if Style_Check and then Token = Tok_In then
            Style.Check_Not_In;
         end if;

         T_In;
      end if;

      return Op_Kind;
   end P_Relational_Operator;

   ---------------------------------
   -- 4.5  Binary Adding Operator --
   ---------------------------------

   --  BINARY_ADDING_OPERATOR ::= + | - | &

   --  The value returned is the appropriate Node_Kind code for the operator.
   --  On return, Token points to the operator token (NOT past it).

   --  The caller has checked that the first token is a legitimate adding
   --  operator token (i.e. is one of the operator tokens listed above).

   --  Error recovery: cannot raise Error_Resync

   function P_Binary_Adding_Operator return Node_Kind is
      Addop_Node : constant array (Token_Class_Binary_Addop) of Node_Kind :=
                     (Tok_Ampersand => N_Op_Concat,
                      Tok_Minus     => N_Op_Subtract,
                      Tok_Plus      => N_Op_Add);
   begin
      return Addop_Node (Token);
   end P_Binary_Adding_Operator;

   --------------------------------
   -- 4.5  Unary Adding Operator --
   --------------------------------

   --  UNARY_ADDING_OPERATOR ::= + | -

   --  The value returned is the appropriate Node_Kind code for the operator.
   --  On return, Token points to the operator token (NOT past it).

   --  The caller has checked that the first token is a legitimate adding
   --  operator token (i.e. is one of the operator tokens listed above).

   --  Error recovery: cannot raise Error_Resync

   function P_Unary_Adding_Operator return Node_Kind is
      Addop_Node : constant array (Token_Class_Unary_Addop) of Node_Kind :=
                     (Tok_Minus => N_Op_Minus,
                      Tok_Plus  => N_Op_Plus);
   begin
      return Addop_Node (Token);
   end P_Unary_Adding_Operator;

   -------------------------------
   -- 4.5  Multiplying Operator --
   -------------------------------

   --  MULTIPLYING_OPERATOR ::= * | / | mod | rem

   --  The value returned is the appropriate Node_Kind code for the operator.
   --  On return, Token points to the operator token (NOT past it).

   --  The caller has checked that the first token is a legitimate multiplying
   --  operator token (i.e. is one of the operator tokens listed above).

   --  Error recovery: cannot raise Error_Resync

   function P_Multiplying_Operator return Node_Kind is
      Mulop_Node : constant array (Token_Class_Mulop) of Node_Kind :=
        (Tok_Asterisk       => N_Op_Multiply,
         Tok_Mod            => N_Op_Mod,
         Tok_Rem            => N_Op_Rem,
         Tok_Slash          => N_Op_Divide);
   begin
      return Mulop_Node (Token);
   end P_Multiplying_Operator;

   --------------------------------------
   -- 4.5  Highest Precedence Operator --
   --------------------------------------

   --  Parsed by P_Factor (4.4)

   --  Note: this rule is not in fact used by the grammar at any point!

   --------------------------
   -- 4.6  Type Conversion --
   --------------------------

   --  Parsed by P_Primary as a Name (4.1)

   -------------------------------
   -- 4.7  Qualified Expression --
   -------------------------------

   --  QUALIFIED_EXPRESSION ::=
   --    SUBTYPE_MARK ' (EXPRESSION) | SUBTYPE_MARK ' AGGREGATE

   --  The caller has scanned the name which is the Subtype_Mark parameter
   --  and scanned past the single quote following the subtype mark. The
   --  caller has not checked that this name is in fact appropriate for
   --  a subtype mark name (i.e. it is a selected component or identifier).

   --  Error_Recovery: cannot raise Error_Resync

   function P_Qualified_Expression (Subtype_Mark : Node_Id) return Node_Id is
      Qual_Node : Node_Id;
   begin
      Qual_Node := New_Node (N_Qualified_Expression, Prev_Token_Ptr);
      Set_Subtype_Mark (Qual_Node, Check_Subtype_Mark (Subtype_Mark));
      Set_Expression (Qual_Node, P_Aggregate_Or_Paren_Expr);
      return Qual_Node;
   end P_Qualified_Expression;

   --------------------
   -- 4.8  Allocator --
   --------------------

   --  ALLOCATOR ::=
   --      new [SUBPOOL_SPECIFICATION] SUBTYPE_INDICATION
   --    | new [SUBPOOL_SPECIFICATION] QUALIFIED_EXPRESSION
   --
   --  SUBPOOL_SPECIFICATION ::= (subpool_handle_NAME)

   --  The caller has checked that the initial token is NEW

   --  Error recovery: can raise Error_Resync

   function P_Allocator return Node_Id is
      Alloc_Node             : Node_Id;
      Type_Node              : Node_Id;
      Null_Exclusion_Present : Boolean;

   begin
      Alloc_Node := New_Node (N_Allocator, Token_Ptr);
      T_New;

      --  Scan subpool_specification if present (Ada 2012 (AI05-0111-3))

      --  Scan Null_Exclusion if present (Ada 2005 (AI-231))

      if Token = Tok_Left_Paren then
         Scan; -- past (
         Set_Subpool_Handle_Name (Alloc_Node, P_Name);
         T_Right_Paren;

         if Ada_Version < Ada_2012 then
            Error_Msg_N
              ("|subpool specification is an Ada 2012 feature",
               Subpool_Handle_Name (Alloc_Node));
            Error_Msg_N
              ("\|unit must be compiled with -gnat2012 switch",
               Subpool_Handle_Name (Alloc_Node));
         end if;
      end if;

      Null_Exclusion_Present := P_Null_Exclusion;
      Set_Null_Exclusion_Present (Alloc_Node, Null_Exclusion_Present);
      Type_Node := P_Subtype_Mark_Resync;

      if Token = Tok_Apostrophe then
         Scan; -- past apostrophe
         Set_Expression (Alloc_Node, P_Qualified_Expression (Type_Node));
      else
         Set_Expression
           (Alloc_Node,
            P_Subtype_Indication (Type_Node, Null_Exclusion_Present));

         --  AI05-0104: An explicit null exclusion is not allowed for an
         --  allocator without initialization. In previous versions of the
         --  language it just raises constraint error.

         if Ada_Version >= Ada_2012 and then Null_Exclusion_Present then
            Error_Msg_N
              ("an allocator with a subtype indication "
               & "cannot have a null exclusion", Alloc_Node);
         end if;
      end if;

      return Alloc_Node;
   end P_Allocator;

   -----------------------
   -- P_Case_Expression --
   -----------------------

   function P_Case_Expression return Node_Id is
      Loc        : constant Source_Ptr := Token_Ptr;
      Case_Node  : Node_Id;
      Save_State : Saved_Scan_State;

   begin
      if Ada_Version < Ada_2012 then
         Error_Msg_SC ("|case expression is an Ada 2012 feature");
         Error_Msg_SC ("\|unit must be compiled with -gnat2012 switch");
      end if;

      Scan; -- past CASE
      Case_Node :=
        Make_Case_Expression (Loc,
          Expression   => P_Expression_No_Right_Paren,
          Alternatives => New_List);
      T_Is;

      --  We now have scanned out CASE expression IS, scan alternatives

      loop
         T_When;
         Append_To (Alternatives (Case_Node), P_Case_Expression_Alternative);

         --  Missing comma if WHEN (more alternatives present)

         if Token = Tok_When then
            T_Comma;

         --  If comma/WHEN, skip comma and we have another alternative

         elsif Token = Tok_Comma then
            Save_Scan_State (Save_State);
            Scan; -- past comma

            if Token /= Tok_When then
               Restore_Scan_State (Save_State);
               exit;
            end if;

         --  If no comma or WHEN, definitely done

         else
            exit;
         end if;
      end loop;

      --  If we have an END CASE, diagnose as not needed

      if Token = Tok_End then
         Error_Msg_SC ("`END CASE` not allowed at end of case expression");
         Scan; -- past END

         if Token = Tok_Case then
            Scan; -- past CASE;
         end if;
      end if;

      --  Return the Case_Expression node

      return Case_Node;
   end P_Case_Expression;

   -----------------------------------
   -- P_Case_Expression_Alternative --
   -----------------------------------

   --  CASE_STATEMENT_ALTERNATIVE ::=
   --    when DISCRETE_CHOICE_LIST =>
   --      EXPRESSION

   --  The caller has checked that and scanned past the initial WHEN token
   --  Error recovery: can raise Error_Resync

   function P_Case_Expression_Alternative return Node_Id is
      Case_Alt_Node : Node_Id;
   begin
      Case_Alt_Node := New_Node (N_Case_Expression_Alternative, Token_Ptr);
      Set_Discrete_Choices (Case_Alt_Node, P_Discrete_Choice_List);
      TF_Arrow;
      Set_Expression (Case_Alt_Node, P_Expression);
      return Case_Alt_Node;
   end P_Case_Expression_Alternative;

   ---------------------
   -- P_If_Expression --
   ---------------------

   function P_If_Expression return Node_Id is
      Exprs : constant List_Id    := New_List;
      Loc   : constant Source_Ptr := Token_Ptr;
      Expr  : Node_Id;
      State : Saved_Scan_State;

   begin
      Inside_If_Expression := Inside_If_Expression + 1;

      if Token = Tok_If and then Ada_Version < Ada_2012 then
         Error_Msg_SC ("|if expression is an Ada 2012 feature");
         Error_Msg_SC ("\|unit must be compiled with -gnat2012 switch");
      end if;

      Scan; -- past IF or ELSIF
      Append_To (Exprs, P_Condition);
      TF_Then;
      Append_To (Exprs, P_Expression);

      --  We now have scanned out IF expr THEN expr

      --  Check for common error of semicolon before the ELSE

      if Token = Tok_Semicolon then
         Save_Scan_State (State);
         Scan; -- past semicolon

         if Token = Tok_Else or else Token = Tok_Elsif then
            Error_Msg_SP -- CODEFIX
              ("|extra "";"" ignored");

         else
            Restore_Scan_State (State);
         end if;
      end if;

      --  Scan out ELSIF sequence if present

      if Token = Tok_Elsif then
         Expr := P_If_Expression;
         Set_Is_Elsif (Expr);
         Append_To (Exprs, Expr);

      --  Scan out ELSE phrase if present

      elsif Token = Tok_Else then

         --  Scan out ELSE expression

         Scan; -- Past ELSE
         Append_To (Exprs, P_Expression);

      --  Two expression case (implied True, filled in during semantics)

      else
         null;
      end if;

      --  If we have an END IF, diagnose as not needed

      if Token = Tok_End then
         Error_Msg_SC ("`END IF` not allowed at end of if expression");
         Scan; -- past END

         if Token = Tok_If then
            Scan; -- past IF;
         end if;
      end if;

      Inside_If_Expression := Inside_If_Expression - 1;

      --  Return the If_Expression node

      return
        Make_If_Expression (Loc,
          Expressions => Exprs);
   end P_If_Expression;

   -----------------------
   -- P_Membership_Test --
   -----------------------

   --  MEMBERSHIP_CHOICE_LIST ::= MEMBERHIP_CHOICE {'|' MEMBERSHIP_CHOICE}
   --  MEMBERSHIP_CHOICE      ::= CHOICE_EXPRESSION | range | subtype_mark

   procedure P_Membership_Test (N : Node_Id) is
      Alt : constant Node_Id :=
              P_Range_Or_Subtype_Mark
                (Allow_Simple_Expression => (Ada_Version >= Ada_2012));

   begin
      --  Set case

      if Token = Tok_Vertical_Bar then
         if Ada_Version < Ada_2012 then
            Error_Msg_SC ("set notation is an Ada 2012 feature");
            Error_Msg_SC ("\|unit must be compiled with -gnat2012 switch");
         end if;

         Set_Alternatives (N, New_List (Alt));
         Set_Right_Opnd   (N, Empty);

         --  Loop to accumulate alternatives

         while Token = Tok_Vertical_Bar loop
            Scan; -- past vertical bar
            Append_To
              (Alternatives (N),
               P_Range_Or_Subtype_Mark (Allow_Simple_Expression => True));
         end loop;

      --  Not set case

      else
         Set_Right_Opnd   (N, Alt);
         Set_Alternatives (N, No_List);
      end if;
   end P_Membership_Test;

   ------------------------------------------
   -- P_Unparen_Cond_Case_Quant_Expression --
   ------------------------------------------

   function P_Unparen_Cond_Case_Quant_Expression return Node_Id is
      Lparen : constant Boolean := Prev_Token = Tok_Left_Paren;
      Result : Node_Id;

   begin
      --  Case expression

      if Token = Tok_Case then
         Result := P_Case_Expression;

         if not (Lparen and then Token = Tok_Right_Paren) then
            Error_Msg_N ("case expression must be parenthesized!", Result);
         end if;

      --  If expression

      elsif Token = Tok_If then
         Result := P_If_Expression;

         if not (Lparen and then Token = Tok_Right_Paren) then
            Error_Msg_N ("if expression must be parenthesized!", Result);
         end if;

      --  Quantified expression

      elsif Token = Tok_For then
         Result := P_Quantified_Expression;

         if not (Lparen and then Token = Tok_Right_Paren) then
            Error_Msg_N
              ("quantified expression must be parenthesized!", Result);
         end if;

      --  No other possibility should exist (caller was supposed to check)

      else
         raise Program_Error;
      end if;

      --  Return expression (possibly after having given message)

      return Result;
   end P_Unparen_Cond_Case_Quant_Expression;

end Ch4;
