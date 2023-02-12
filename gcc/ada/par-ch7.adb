------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P A R . C H 7                               --
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

pragma Style_Checks (All_Checks);
--  Turn off subprogram body ordering check. Subprograms are in order
--  by RM section rather than alphabetical

separate (Par)
package body Ch7 is

   ---------------------------------------------
   -- 7.1  Package (also 8.5.3, 10.1.3, 12.3) --
   ---------------------------------------------

   --  This routine scans out a package declaration, package body, or a
   --  renaming declaration or generic instantiation starting with PACKAGE

   --  PACKAGE_DECLARATION ::=
   --    PACKAGE_SPECIFICATION;

   --  PACKAGE_SPECIFICATION ::=
   --    package DEFINING_PROGRAM_UNIT_NAME
   --      [ASPECT_SPECIFICATIONS]
   --    is
   --      {BASIC_DECLARATIVE_ITEM}
   --    [private
   --      {BASIC_DECLARATIVE_ITEM}]
   --    end [[PARENT_UNIT_NAME .] IDENTIFIER]

   --  PACKAGE_BODY ::=
   --    package body DEFINING_PROGRAM_UNIT_NAME
   --      [ASPECT_SPECIFICATIONS]
   --    is
   --      DECLARATIVE_PART
   --    [begin
   --      HANDLED_SEQUENCE_OF_STATEMENTS]
   --    end [[PARENT_UNIT_NAME .] IDENTIFIER]

   --  PACKAGE_RENAMING_DECLARATION ::=
   --    package DEFINING_IDENTIFIER renames package_NAME
   --      [ASPECT_SPECIFICATIONS];

   --  PACKAGE_BODY_STUB ::=
   --    package body DEFINING_IDENTIFIER is separate
   --      [ASPECT_SPECIFICATIONS];

   --  PACKAGE_INSTANTIATION ::=
   --    package DEFINING_PROGRAM_UNIT_NAME is
   --      new generic_package_NAME [GENERIC_ACTUAL_PART]
   --        [ASPECT_SPECIFICATIONS];

   --  Note: in all contexts where a package specification is required, there
   --  is a terminating semicolon. This semicolon is scanned out in the case
   --  where Pf_Flags is set to Pf_Spcn, even though it is not strictly part
   --  of the package specification (it's just too much trouble, and really
   --  quite unnecessary, to deal with scanning out an END where the semicolon
   --  after the END is not considered to be part of the END.

   --  The caller has checked that the initial token is PACKAGE

   --  Error recovery: cannot raise Error_Resync

   function P_Package (Pf_Flags : Pf_Rec) return Node_Id is
      Package_Node       : Node_Id;
      Specification_Node : Node_Id;
      Name_Node          : Node_Id;
      Package_Sloc       : Source_Ptr;

      Aspect_Sloc : Source_Ptr := No_Location;
      --  Save location of WITH for scanned aspects. Left set to No_Location
      --  if no aspects scanned before the IS keyword.

      Is_Sloc : Source_Ptr;
      --  Save location of IS token for package declaration

      Dummy_Node : constant Node_Id :=
                     New_Node (N_Package_Specification, Token_Ptr);
      --  Dummy node to attach aspect specifications to until we properly
      --  figure out where they eventually belong.

   begin
      Push_Scope_Stack;
      Scopes (Scope.Last).Etyp := E_Name;
      Scopes (Scope.Last).Ecol := Start_Column;
      Scopes (Scope.Last).Lreq := False;

      Package_Sloc := Token_Ptr;
      Scan; -- past PACKAGE

      if Token = Tok_Type then
         Error_Msg_SC -- CODEFIX
           ("TYPE not allowed here");
         Scan; -- past TYPE
      end if;

      --  Case of package body. Note that we demand a package body if that
      --  is the only possibility (even if the BODY keyword is not present)

      if Token = Tok_Body or else Pf_Flags = Pf_Pbod_Pexp then
         if not Pf_Flags.Pbod then
            Error_Msg_SC ("package body cannot appear here!");
         end if;

         T_Body;
         Scopes (Scope.Last).Sloc := Token_Ptr;
         Name_Node := P_Defining_Program_Unit_Name;
         Scopes (Scope.Last).Labl := Name_Node;
         Current_Node := Name_Node;

         if Aspect_Specifications_Present then
            Aspect_Sloc := Token_Ptr;
            P_Aspect_Specifications (Dummy_Node, Semicolon => False);
         end if;

         TF_Is;

         if Separate_Present then
            if not Pf_Flags.Stub then
               Error_Msg_SC ("body stub cannot appear here!");
            end if;

            Scan; -- past SEPARATE

            Package_Node := New_Node (N_Package_Body_Stub, Package_Sloc);
            Set_Defining_Identifier (Package_Node, Name_Node);

            if Has_Aspects (Dummy_Node) then
               Error_Msg
                 ("aspect specifications must come after SEPARATE",
                  Aspect_Sloc);
            end if;

            P_Aspect_Specifications (Package_Node, Semicolon => False);
            TF_Semicolon;
            Pop_Scope_Stack;

         else
            Package_Node := New_Node (N_Package_Body, Package_Sloc);
            Set_Defining_Unit_Name (Package_Node, Name_Node);

            --  Move the aspect specifications to the body node

            if Has_Aspects (Dummy_Node) then
               Move_Aspects (From => Dummy_Node, To => Package_Node);
            end if;

            Parse_Decls_Begin_End (Package_Node);
         end if;

      --  Cases other than Package_Body

      else
         Scopes (Scope.Last).Sloc := Token_Ptr;
         Name_Node := P_Defining_Program_Unit_Name;
         Scopes (Scope.Last).Labl := Name_Node;
         Current_Node := Name_Node;

         --  Case of renaming declaration

         Check_Misspelling_Of (Tok_Renames);

         if Token = Tok_Renames then
            if not Pf_Flags.Rnam then
               Error_Msg_SC ("renaming declaration cannot appear here!");
            end if;

            Scan; -- past RENAMES;

            Package_Node :=
              New_Node (N_Package_Renaming_Declaration, Package_Sloc);
            Set_Defining_Unit_Name (Package_Node, Name_Node);
            Set_Name (Package_Node, P_Qualified_Simple_Name);

            No_Constraint;
            P_Aspect_Specifications (Package_Node, Semicolon => False);
            TF_Semicolon;
            Pop_Scope_Stack;

         --  Generic package instantiation or package declaration

         else
            if Aspect_Specifications_Present then
               Aspect_Sloc := Token_Ptr;
               P_Aspect_Specifications (Dummy_Node, Semicolon => False);
            end if;

            Is_Sloc := Token_Ptr;
            TF_Is;

            --  Case of generic instantiation

            if Token = Tok_New then
               if not Pf_Flags.Gins then
                  Error_Msg_SC
                     ("generic instantiation cannot appear here!");
               end if;

               if Aspect_Sloc /= No_Location then
                  Error_Msg
                    ("misplaced aspects for package instantiation",
                     Aspect_Sloc);
               end if;

               Scan; -- past NEW

               Package_Node :=
                 New_Node (N_Package_Instantiation, Package_Sloc);
               Set_Defining_Unit_Name (Package_Node, Name_Node);
               Set_Name (Package_Node, P_Qualified_Simple_Name);
               Set_Generic_Associations
                 (Package_Node, P_Generic_Actual_Part_Opt);

               if Aspect_Sloc /= No_Location
                 and then not Aspect_Specifications_Present
               then
                  Error_Msg_SC ("info: aspect specifications belong here??");
                  Move_Aspects (From => Dummy_Node, To => Package_Node);
               end if;

               P_Aspect_Specifications (Package_Node);
               Pop_Scope_Stack;

            --  Case of package declaration or package specification

            else
               Specification_Node :=
                 New_Node (N_Package_Specification, Package_Sloc);

               Set_Defining_Unit_Name (Specification_Node, Name_Node);
               Set_Visible_Declarations
                 (Specification_Node,
                  P_Basic_Declarative_Items (Declare_Expression => False));

               if Token = Tok_Private then
                  Error_Msg_Col := Scopes (Scope.Last).Ecol;

                  if RM_Column_Check then
                     if Token_Is_At_Start_Of_Line
                       and then Start_Column /= Error_Msg_Col
                     then
                        Error_Msg_SC
                          ("(style) PRIVATE in wrong column, should be@");
                     end if;
                  end if;

                  Scan; -- past PRIVATE

                  Set_Private_Declarations
                    (Specification_Node,
                     P_Basic_Declarative_Items (Declare_Expression => False));

                  --  Deal gracefully with multiple PRIVATE parts

                  while Token = Tok_Private loop
                     Error_Msg_SC
                       ("only one private part allowed per package");
                     Scan; -- past PRIVATE
                     Append_List
                       (P_Basic_Declarative_Items
                          (Declare_Expression => False),
                        Private_Declarations (Specification_Node));
                  end loop;
               end if;

               if Pf_Flags = Pf_Spcn then
                  Package_Node := Specification_Node;
               else
                  Package_Node :=
                    New_Node (N_Package_Declaration, Package_Sloc);
                  Set_Specification (Package_Node, Specification_Node);
               end if;

               if Token = Tok_Begin then
                  Error_Msg_SC ("begin block not allowed in package spec");
                  Scan; -- past BEGIN
                  Discard_Junk_List (P_Sequence_Of_Statements (SS_None));
               end if;

               End_Statements (Specification_Node, Empty, Is_Sloc);
               Move_Aspects (From => Dummy_Node, To => Package_Node);
            end if;
         end if;
      end if;

      return Package_Node;
   end P_Package;

   ------------------------------
   -- 7.1  Package Declaration --
   ------------------------------

   --  Parsed by P_Package (7.1)

   --------------------------------
   -- 7.1  Package Specification --
   --------------------------------

   --  Parsed by P_Package (7.1)

   -----------------------
   -- 7.1  Package Body --
   -----------------------

   --  Parsed by P_Package (7.1)

   -----------------------------------
   -- 7.3  Private Type Declaration --
   -----------------------------------

   --  Parsed by P_Type_Declaration (3.2.1)

   ----------------------------------------
   -- 7.3  Private Extension Declaration --
   ----------------------------------------

   --  Parsed by P_Type_Declaration (3.2.1)

end Ch7;
