------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  S C N                                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2020, Free Software Foundation, Inc.         --
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
with Csets;    use Csets;
with Namet;    use Namet;
with Opt;      use Opt;
with Restrict; use Restrict;
with Rident;   use Rident;
with Scans;    use Scans;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Uintp;    use Uintp;

package body Scn is

   Used_As_Identifier : array (Token_Type) of Boolean;
   --  Flags set True if a given keyword is used as an identifier (used to
   --  make sure that we only post an error message for incorrect use of a
   --  keyword as an identifier once for a given keyword).

   ----------------------------
   -- Determine_Token_Casing --
   ----------------------------

   function Determine_Token_Casing return Casing_Type is
   begin
      return Scanner.Determine_Token_Casing;
   end Determine_Token_Casing;

   ------------------------
   -- Initialize_Scanner --
   ------------------------

   procedure Initialize_Scanner
     (Unit  : Unit_Number_Type;
      Index : Source_File_Index) is
   begin
      Scanner.Initialize_Scanner (Index);
      Set_Unit (Index, Unit);

      Current_Source_Unit := Unit;

      --  Set default for Comes_From_Source. All nodes built now until we
      --  reenter the analyzer will have Comes_From_Source set to True

      Set_Comes_From_Source_Default (True);

      Check_For_BOM;

      --  Because of the License stuff above, Scng.Initialize_Scanner cannot
      --  call Scan. Scan initial token (note this initializes Prev_Token,
      --  Prev_Token_Ptr).

      Scan;

      --  Clear flags for reserved words used as identifiers

      Used_As_Identifier := (others => False);
   end Initialize_Scanner;

   ---------------
   -- Post_Scan --
   ---------------

   procedure Post_Scan is
      procedure Check_Obsolescent_Features_Restriction (S : Source_Ptr);
      --  This checks for Obsolescent_Features restriction being active, and
      --  if so, flags the restriction as occurring at the given scan location.

      procedure Check_Obsolete_Base_Char;
      --  Check for numeric literal using ':' instead of '#' for based case

      --------------------------------------------
      -- Check_Obsolescent_Features_Restriction --
      --------------------------------------------

      procedure Check_Obsolescent_Features_Restriction (S : Source_Ptr) is
      begin
         --  Normally we have a node handy for posting restrictions. We don't
         --  have such a node here, so construct a dummy one with the right
         --  scan pointer. This is only used to get the Sloc value anyway.

         Check_Restriction (No_Obsolescent_Features, New_Node (N_Empty, S));
      end Check_Obsolescent_Features_Restriction;

      ------------------------------
      -- Check_Obsolete_Base_Char --
      ------------------------------

      procedure Check_Obsolete_Base_Char is
         S : Source_Ptr;

      begin
         if Based_Literal_Uses_Colon then

            --  Find the : for the restriction or warning message

            S := Token_Ptr;
            while Source (S) /= ':' loop
               S := S + 1;
            end loop;

            Check_Obsolescent_Features_Restriction (S);

            if Warn_On_Obsolescent_Feature then
               Error_Msg
                 ("?j?use of "":"" is an obsolescent feature (RM J.2(3))", S);
               Error_Msg
                 ("\?j?use ""'#"" instead", S);
            end if;
         end if;
      end Check_Obsolete_Base_Char;

   --  Start of processing for Post_Scan

   begin
      case Token is
         when Tok_Char_Literal =>
            Token_Node := New_Node (N_Character_Literal, Token_Ptr);
            Set_Char_Literal_Value (Token_Node, UI_From_CC (Character_Code));
            Set_Chars (Token_Node, Token_Name);

         when Tok_Identifier =>
            Token_Node := New_Node (N_Identifier, Token_Ptr);
            Set_Chars (Token_Node, Token_Name);

         when Tok_Real_Literal =>
            Token_Node := New_Node (N_Real_Literal, Token_Ptr);
            Set_Realval (Token_Node, Real_Literal_Value);
            Check_Obsolete_Base_Char;

         when Tok_Integer_Literal =>
            Token_Node := New_Node (N_Integer_Literal, Token_Ptr);
            Set_Intval (Token_Node, Int_Literal_Value);
            Check_Obsolete_Base_Char;

         when Tok_String_Literal =>
            Token_Node := New_Node (N_String_Literal, Token_Ptr);
            Set_Has_Wide_Character
              (Token_Node, Wide_Character_Found);
            Set_Has_Wide_Wide_Character
              (Token_Node, Wide_Wide_Character_Found);
            Set_Strval (Token_Node, String_Literal_Id);

            if Source (Token_Ptr) = '%' then
               Check_Obsolescent_Features_Restriction (Token_Ptr);

               if Warn_On_Obsolescent_Feature then
                  Error_Msg_SC
                    ("?j?use of ""'%"" is an obsolescent feature (RM J.2(4))");
                  Error_Msg_SC ("\?j?use """""" instead");
               end if;
            end if;

         when Tok_Operator_Symbol =>
            Token_Node := New_Node (N_Operator_Symbol, Token_Ptr);
            Set_Chars (Token_Node, Token_Name);
            Set_Strval (Token_Node, String_Literal_Id);

         when Tok_Vertical_Bar =>
            if Source (Token_Ptr) = '!' then
               Check_Obsolescent_Features_Restriction (Token_Ptr);

               if Warn_On_Obsolescent_Feature then
                  Error_Msg_SC
                    ("?j?use of ""'!"" is an obsolescent feature (RM J.2(2))");
                  Error_Msg_SC ("\?j?use ""'|"" instead");
               end if;
            end if;

         when others =>
            null;
      end case;
   end Post_Scan;

   ------------------------------
   -- Scan_Reserved_Identifier --
   ------------------------------

   procedure Scan_Reserved_Identifier (Force_Msg : Boolean) is
      Token_Chars : String  := Token_Type'Image (Token);
      Len         : Natural := 0;

   begin
      --  AI12-0125 : '@' denotes the target_name, i.e. serves as an
      --  abbreviation for the LHS of an assignment.

      if Token = Tok_At_Sign then
         Token_Node := New_Node (N_Target_Name, Token_Ptr);
         return;
      end if;

      --  We have in Token_Chars the image of the Token name, i.e. Tok_xxx.
      --  This code extracts the xxx and makes an identifier out of it.

      for J in 5 .. Token_Chars'Length loop
         Len := Len + 1;
         Token_Chars (Len) := Fold_Lower (Token_Chars (J));
      end loop;

      Token_Name := Name_Find (Token_Chars (1 .. Len));

      --  If Inside_Pragma is True, we don't give an error. This is to allow
      --  things like "pragma Ignore_Pragma (Interface)", where "Interface" is
      --  a reserved word. There is no danger of missing errors, because any
      --  misuse must have been preceded by an illegal declaration. For
      --  example, in "pragma Pack (Begin);", either Begin is not declared,
      --  which is an error, or it is declared, which will be an error on that
      --  declaration.

      if (not Used_As_Identifier (Token) or else Force_Msg)
        and then not Inside_Pragma
      then
         Error_Msg_Name_1 := Token_Name;
         Error_Msg_SC ("reserved word* cannot be used as identifier!");
         Used_As_Identifier (Token) := True;
      end if;

      Token := Tok_Identifier;
      Token_Node := New_Node (N_Identifier, Token_Ptr);
      Set_Chars (Token_Node, Token_Name);
   end Scan_Reserved_Identifier;

end Scn;
