------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P A R . C H 8                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2004 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
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

   function P_Use_Package_Clause                           return Node_Id;
   function P_Use_Type_Clause                              return Node_Id;

   ---------------------
   -- 8.4  Use Clause --
   ---------------------

   --  USE_CLAUSE ::= USE_PACKAGE_CLAUSE | USE_TYPE_CLAUSE

   --  The caller has checked that the initial token is USE

   --  Error recovery: cannot raise Error_Resync

   function P_Use_Clause return Node_Id is
   begin
      Scan; -- past USE

      if Token = Tok_Type then
         return P_Use_Type_Clause;

      else
         return P_Use_Package_Clause;
      end if;
   end P_Use_Clause;

   -----------------------------
   -- 8.4  Use Package Clause --
   -----------------------------

   --  USE_PACKAGE_CLAUSE ::= use package_NAME {, package_NAME};

   --  The caller has scanned out the USE keyword

   --  Error recovery: cannot raise Error_Resync

   function P_Use_Package_Clause return Node_Id is
      Use_Node : Node_Id;

   begin
      Use_Node := New_Node (N_Use_Package_Clause, Prev_Token_Ptr);
      Set_Names (Use_Node, New_List);

      if Token = Tok_Package then
         Error_Msg_SC ("PACKAGE should not appear here");
         Scan; -- past PACKAGE
      end if;

      loop
         Append (P_Qualified_Simple_Name, Names (Use_Node));
         exit when Token /= Tok_Comma;
         Scan; -- past comma
      end loop;

      TF_Semicolon;
      return Use_Node;
   end P_Use_Package_Clause;

   --------------------------
   -- 8.4  Use Type Clause --
   --------------------------

   --  USE_TYPE_CLAUSE ::= use type SUBTYPE_MARK {, SUBTYPE_MARK};

   --  The caller has checked that the initial token is USE, scanned it out
   --  and that the current token is TYPE.

   --  Error recovery: cannot raise Error_Resync

   function P_Use_Type_Clause return Node_Id is
      Use_Node : Node_Id;

   begin
      Use_Node := New_Node (N_Use_Type_Clause, Prev_Token_Ptr);
      Set_Subtype_Marks (Use_Node, New_List);

      if Ada_Version = Ada_83 then
         Error_Msg_SC ("(Ada 83) use type not allowed!");
      end if;

      Scan; -- past TYPE

      loop
         Append (P_Subtype_Mark, Subtype_Marks (Use_Node));
         No_Constraint;
         exit when Token /= Tok_Comma;
         Scan; -- past comma
      end loop;

      TF_Semicolon;
      return Use_Node;
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

   ----------------------------------------
   -- 8.5.2  Exception Renaming Declaration --
   ----------------------------------------

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
