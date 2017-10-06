------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P A R . C H 8                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2017, Free Software Foundation, Inc.         --
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
package body Ch8 is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Append_Use_Clause
     (Item_List : List_Id;
      Use_Node  : Node_Id;
      Is_First  : in out Boolean;
      Is_Last   : in out Boolean);
   --  Append a use_clause to the Item_List, appropriately setting the Prev_Ids
   --  and More_Ids flags for each split use node. The flags Is_First and
   --  Is_Last track position of subtype_marks or names within the original
   --  use_clause.

   procedure P_Use_Package_Clause (Item_List : List_Id);
   procedure P_Use_Type_Clause    (Item_List : List_Id);

   -----------------------
   -- Append_Use_Clause --
   -----------------------

   procedure Append_Use_Clause
     (Item_List : List_Id;
      Use_Node  : Node_Id;
      Is_First  : in out Boolean;
      Is_Last   : in out Boolean)
   is
   begin
      if Token /= Tok_Comma then
         if not Is_First then
            Set_Prev_Ids (Use_Node);
         end if;

         Append (Use_Node, Item_List);
         Is_Last := True;
      else
         Set_More_Ids (Use_Node);

         if not Is_First then
            Set_Prev_Ids (Use_Node);
         else
            Is_First := False;
         end if;

         Append (Use_Node, Item_List);
         Scan; --  Past comma
      end if;
   end Append_Use_Clause;

   ---------------------
   -- 8.4  Use Clause --
   ---------------------

   --  USE_CLAUSE ::= USE_PACKAGE_CLAUSE | USE_TYPE_CLAUSE

   --  The caller has checked that the initial token is USE

   --  Error recovery: cannot raise Error_Resync

   procedure P_Use_Clause (Item_List : List_Id) is
   begin
      Scan; -- past USE

      if Token = Tok_Type or else Token = Tok_All then
         P_Use_Type_Clause (Item_List);
      else
         P_Use_Package_Clause (Item_List);
      end if;
   end P_Use_Clause;

   -----------------------------
   -- 8.4  Use Package Clause --
   -----------------------------

   --  USE_PACKAGE_CLAUSE ::= use package_NAME {, package_NAME};

   --  The caller has scanned out the USE keyword

   --  Error recovery: cannot raise Error_Resync

   procedure P_Use_Package_Clause (Item_List : List_Id) is
      Is_First : Boolean := True;
      Is_Last  : Boolean := False;
      Use_Node : Node_Id;
      Use_Sloc : constant Source_Ptr := Prev_Token_Ptr;

   begin
      if Token = Tok_Package then
         Error_Msg_SC ("PACKAGE should not appear here");
         Scan; --  Past PACKAGE
      end if;

      --  Loop through names in a single use_package_clause, generating an
      --  N_Use_Package_Clause node for each name encountered.

      loop
         Use_Node := New_Node (N_Use_Package_Clause, Use_Sloc);
         Set_Name (Use_Node, P_Qualified_Simple_Name);

         --  Locally chain each name's use-package node

         Append_Use_Clause (Item_List, Use_Node, Is_First, Is_Last);
         exit when Is_Last;
      end loop;

      TF_Semicolon;
   end P_Use_Package_Clause;

   --------------------------
   -- 8.4  Use Type Clause --
   --------------------------

   --  USE_TYPE_CLAUSE ::= use [ALL] type SUBTYPE_MARK {, SUBTYPE_MARK};

   --  The caller has checked that the initial token is USE, scanned it out
   --  and that the current token is either ALL or TYPE.

   --  Note: Use of ALL is an Ada 2012 feature

   --  Error recovery: cannot raise Error_Resync

   procedure P_Use_Type_Clause (Item_List : List_Id) is
      All_Present : Boolean;
      Is_First    : Boolean := True;
      Is_Last     : Boolean := False;
      Use_Node    : Node_Id;
      Use_Sloc    : constant Source_Ptr := Prev_Token_Ptr;

   begin
      if Token = Tok_All then
         Error_Msg_Ada_2012_Feature ("|`USE ALL TYPE`", Token_Ptr);
         All_Present := True;
         Scan; --  Past ALL

         if Token /= Tok_Type then
            Error_Msg_SC ("TYPE expected");
         end if;

      else
         pragma Assert (Token = Tok_Type);
         All_Present := False;
      end if;

      if Ada_Version = Ada_83 then
         Error_Msg_SC ("(Ada 83) use type not allowed!");
      end if;

      Scan; --  Past TYPE

      --  Loop through subtype_marks in one use_type_clause, generating a
      --  separate N_Use_Type_Clause node for each subtype_mark encountered.

      loop
         Use_Node := New_Node (N_Use_Type_Clause, Use_Sloc);
         Set_All_Present (Use_Node, All_Present);
         Set_Used_Operations (Use_Node, No_Elist);

         Set_Subtype_Mark (Use_Node, P_Subtype_Mark);

         No_Constraint;

         --  Locally chain each subtype_mark's use-type node

         Append_Use_Clause (Item_List, Use_Node, Is_First, Is_Last);
         exit when Is_Last;
      end loop;

      TF_Semicolon;
   end P_Use_Type_Clause;

   -------------------------------
   -- 8.5  Renaming Declaration --
   -------------------------------

   --  Object renaming declarations and exception renaming declarations
   --  are parsed by P_Identifier_Declaration (3.3.1)

   --  Subprogram renaming declarations are parsed by P_Subprogram (6.1)

   --  Package renaming declarations are parsed by P_Package (7.1)

   --  Generic renaming declarations are parsed by P_Generic (12.1)

   ----------------------------------------
   -- 8.5.1  Object Renaming Declaration --
   ----------------------------------------

   --  Parsed by P_Identifier_Declarations (3.3.1)

   -------------------------------------------
   -- 8.5.2  Exception Renaming Declaration --
   -------------------------------------------

   --  Parsed by P_Identifier_Declarations (3.3.1)

   -----------------------------------------
   -- 8.5.3  Package Renaming Declaration --
   -----------------------------------------

   --  Parsed by P_Package (7.1)

   --------------------------------------------
   -- 8.5.4  Subprogram Renaming Declaration --
   --------------------------------------------

   --  Parsed by P_Subprogram (6.1)

   -----------------------------------------
   -- 8.5.2  Generic Renaming Declaration --
   -----------------------------------------

   --  Parsed by P_Generic (12.1)

end Ch8;
