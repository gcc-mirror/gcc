------------------------------------------------------------------------------
--                                                                          --
--                          GNAT SYSTEM UTILITIES                           --
--                                                                          --
--                               C S I N F O                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2009, Free Software Foundation, Inc.         --
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

--  Program to check consistency of sinfo.ads and sinfo.adb. Checks that field
--  name usage is consistent and that assertion cross-reference lists are
--  correct, as well as making sure that all the comments on field name usage
--  are consistent.

with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Strings.Maps;              use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants;    use Ada.Strings.Maps.Constants;
with Ada.Text_IO;                   use Ada.Text_IO;

with GNAT.Spitbol;                  use GNAT.Spitbol;
with GNAT.Spitbol.Patterns;         use GNAT.Spitbol.Patterns;
with GNAT.Spitbol.Table_Boolean;
with GNAT.Spitbol.Table_VString;

procedure CSinfo is

   package TB renames GNAT.Spitbol.Table_Boolean;
   package TV renames GNAT.Spitbol.Table_VString;
   use TB, TV;

   Infil  : File_Type;
   Lineno : Natural := 0;

   Err : exception;
   --  Raised on fatal error

   Done : exception;
   --  Raised after error is found to terminate run

   WSP : constant Pattern := Span (' ' & ASCII.HT);

   Fields   : TV.Table (300);
   Fields1  : TV.Table (300);
   Refs     : TV.Table (300);
   Refscopy : TV.Table (300);
   Special  : TB.Table (50);
   Inlines  : TV.Table (100);

   --  The following define the standard fields used for binary operator,
   --  unary operator, and other expression nodes. Numbers in the range 1-5
   --  refer to the Fieldn fields. Letters D-R refer to flags:

   --      D = Flag4
   --      E = Flag5
   --      F = Flag6
   --      G = Flag7
   --      H = Flag8
   --      I = Flag9
   --      J = Flag10
   --      K = Flag11
   --      L = Flag12
   --      M = Flag13
   --      N = Flag14
   --      O = Flag15
   --      P = Flag16
   --      Q = Flag17
   --      R = Flag18

   Flags : TV.Table (20);
   --  Maps flag numbers to letters

   N_Fields : constant Pattern := BreakX ("JL");
   E_Fields : constant Pattern := BreakX ("5EFGHIJLOP");
   U_Fields : constant Pattern := BreakX ("1345EFGHIJKLOPQ");
   B_Fields : constant Pattern := BreakX ("12345EFGHIJKLOPQ");

   Line : VString;
   Bad  : Boolean;

   Field       : constant VString := Nul;
   Fields_Used : VString := Nul;
   Name        : constant VString := Nul;
   Next        : constant VString := Nul;
   Node        : VString := Nul;
   Ref         : VString := Nul;
   Synonym     : constant VString := Nul;
   Nxtref      : constant VString := Nul;

   Which_Field : aliased VString := Nul;

   Node_Search : constant Pattern := WSP & "--  N_" & Rest * Node;
   Break_Punc  : constant Pattern := Break (" .,");
   Plus_Binary : constant Pattern := WSP
                                     & "--  plus fields for binary operator";
   Plus_Unary  : constant Pattern := WSP
                                     & "--  plus fields for unary operator";
   Plus_Expr   : constant Pattern := WSP
                                     & "--  plus fields for expression";
   Break_Syn   : constant Pattern := WSP &  "--  "
                                     & Break (' ') * Synonym
                                     & " (" & Break (')') * Field;
   Break_Field : constant Pattern := BreakX ('-') * Field;
   Get_Field   : constant Pattern := BreakX (Decimal_Digit_Set)
                                     & Span (Decimal_Digit_Set) * Which_Field;
   Break_WFld  : constant Pattern := Break (Which_Field'Access);
   Get_Funcsyn : constant Pattern := WSP & "function " & Rest * Synonym;
   Extr_Field  : constant Pattern := BreakX ('-') & "-- " & Rest * Field;
   Get_Procsyn : constant Pattern := WSP & "procedure Set_" & Rest * Synonym;
   Get_Inline  : constant Pattern := WSP & "pragma Inline ("
                                     & Break (')') * Name;
   Set_Name    : constant Pattern := "Set_" & Rest * Name;
   Func_Rest   : constant Pattern := "   function " & Rest * Synonym;
   Get_Nxtref  : constant Pattern := Break (',') * Nxtref & ',';
   Test_Syn    : constant Pattern := Break ('=') & "= N_"
                                     & (Break (" ,)") or Rest) * Next;
   Chop_Comma  : constant Pattern := BreakX (',') * Next;
   Return_Fld  : constant Pattern := WSP & "return " & Break (' ') * Field;
   Set_Syn     : constant Pattern := "   procedure Set_" & Rest * Synonym;
   Set_Fld     : constant Pattern := WSP & "Set_" & Break (' ') * Field
                                     & " (N, Val)";
   Break_With  : constant Pattern := Break ('_') ** Field & "_With_Parent";

   type VStringA is array (Natural range <>) of VString;

   procedure Next_Line;
   --  Read next line trimmed from Infil into Line and bump Lineno

   procedure Sort (A : in out VStringA);
   --  Sort a (small) array of VString's

   procedure Next_Line is
   begin
      Line := Get_Line (Infil);
      Trim (Line);
      Lineno := Lineno + 1;
   end Next_Line;

   procedure Sort (A : in out VStringA) is
      Temp : VString;
   begin
      <<Sort>>
         for J in 1 .. A'Length - 1 loop
            if A (J) > A (J + 1) then
               Temp := A (J);
               A (J) := A (J + 1);
               A (J + 1) := Temp;
               goto Sort;
            end if;
         end loop;
   end Sort;

--  Start of processing for CSinfo

begin
   Anchored_Mode := True;
   New_Line;
   Open (Infil, In_File, "sinfo.ads");
   Put_Line ("Check for field name consistency");

   --  Setup table for mapping flag numbers to letters

   Set (Flags, "4",  V ("D"));
   Set (Flags, "5",  V ("E"));
   Set (Flags, "6",  V ("F"));
   Set (Flags, "7",  V ("G"));
   Set (Flags, "8",  V ("H"));
   Set (Flags, "9",  V ("I"));
   Set (Flags, "10", V ("J"));
   Set (Flags, "11", V ("K"));
   Set (Flags, "12", V ("L"));
   Set (Flags, "13", V ("M"));
   Set (Flags, "14", V ("N"));
   Set (Flags, "15", V ("O"));
   Set (Flags, "16", V ("P"));
   Set (Flags, "17", V ("Q"));
   Set (Flags, "18", V ("R"));

   --  Special fields table. The following names are not recorded or checked
   --  by Csinfo, since they are specially handled. This means that any field
   --  definition or subprogram with a matching name is ignored.

   Set (Special, "Analyzed",                  True);
   Set (Special, "Assignment_OK",             True);
   Set (Special, "Associated_Node",           True);
   Set (Special, "Cannot_Be_Constant",        True);
   Set (Special, "Chars",                     True);
   Set (Special, "Comes_From_Source",         True);
   Set (Special, "Do_Overflow_Check",         True);
   Set (Special, "Do_Range_Check",            True);
   Set (Special, "Entity",                    True);
   Set (Special, "Entity_Or_Associated_Node", True);
   Set (Special, "Error_Posted",              True);
   Set (Special, "Etype",                     True);
   Set (Special, "Evaluate_Once",             True);
   Set (Special, "First_Itype",               True);
   Set (Special, "Has_Dynamic_Itype",         True);
   Set (Special, "Has_Dynamic_Range_Check",   True);
   Set (Special, "Has_Dynamic_Length_Check",  True);
   Set (Special, "Has_Private_View",          True);
   Set (Special, "Is_Controlling_Actual",     True);
   Set (Special, "Is_Overloaded",             True);
   Set (Special, "Is_Static_Expression",      True);
   Set (Special, "Left_Opnd",                 True);
   Set (Special, "Must_Not_Freeze",           True);
   Set (Special, "Nkind_In",                  True);
   Set (Special, "Parens",                    True);
   Set (Special, "Pragma_Name",               True);
   Set (Special, "Raises_Constraint_Error",   True);
   Set (Special, "Right_Opnd",                True);

   --  Loop to acquire information from node definitions in sinfo.ads,
   --  checking for consistency in Op/Flag assignments to each synonym

   loop
      Bad := False;
      Next_Line;
      exit when Match (Line, "   -- Node Access Functions");

      if Match (Line, Node_Search)
        and then not Match (Node, Break_Punc)
      then
         Fields_Used := Nul;

      elsif Node = "" then
         null;

      elsif Line = "" then
         Node := Nul;

      elsif Match (Line, Plus_Binary) then
         Bad := Match (Fields_Used, B_Fields);

      elsif Match (Line, Plus_Unary) then
         Bad := Match (Fields_Used, U_Fields);

      elsif Match (Line, Plus_Expr) then
         Bad := Match (Fields_Used, E_Fields);

      elsif not Match (Line, Break_Syn) then
         null;

      elsif Match (Synonym, "plus") then
         null;

      else
         Match (Field, Break_Field);

         if not Present (Special, Synonym) then
            if Present (Fields, Synonym) then
               if Field /= Get (Fields, Synonym) then
                  Put_Line
                    ("Inconsistent field reference at line" &
                     Lineno'Img & " for " & Synonym);
                  raise Done;
               end if;

            else
               Set (Fields, Synonym, Field);
            end if;

            Set (Refs, Synonym, Node & ',' & Get (Refs, Synonym));
            Match (Field, Get_Field);

            if Match (Field, "Flag") then
               Which_Field := Get (Flags, Which_Field);
            end if;

            if Match (Fields_Used, Break_WFld) then
               Put_Line
                 ("Overlapping field at line " & Lineno'Img &
                  " for " & Synonym);
               raise Done;
            end if;

            Append (Fields_Used, Which_Field);
            Bad := Bad or Match (Fields_Used, N_Fields);
         end if;
      end if;

      if Bad then
         Put_Line ("fields conflict with standard fields for node " & Node);
         raise Done;
      end if;
   end loop;

   Put_Line ("     OK");
   New_Line;
   Put_Line ("Check for function consistency");

   --  Loop through field function definitions to make sure they are OK

   Fields1 := Fields;
   loop
      Next_Line;
      exit when Match (Line, "   -- Node Update");

      if Match (Line, Get_Funcsyn)
        and then not Present (Special, Synonym)
      then
         if not Present (Fields1, Synonym) then
            Put_Line
              ("function on line " &  Lineno &
               " is for unused synonym");
            raise Done;
         end if;

         Next_Line;

         if not Match (Line, Extr_Field) then
            raise Err;
         end if;

         if Field /= Get (Fields1, Synonym) then
            Put_Line ("Wrong field in function " & Synonym);
            raise Done;

         else
            Delete (Fields1, Synonym);
         end if;
      end if;
   end loop;

   Put_Line ("     OK");
   New_Line;
   Put_Line ("Check for missing functions");

   declare
      List : constant TV.Table_Array := Convert_To_Array (Fields1);

   begin
      if List'Length > 0 then
         Put_Line ("No function for field synonym " & List (1).Name);
         raise Done;
      end if;
   end;

   --  Check field set procedures

   Put_Line ("     OK");
   New_Line;
   Put_Line ("Check for set procedure consistency");

   Fields1 := Fields;
   loop
      Next_Line;
      exit when Match (Line, "   -- Inline Pragmas");
      exit when Match (Line, "   -- Iterator Procedures");

      if Match (Line, Get_Procsyn)
        and then not Present (Special, Synonym)
      then
         if not Present (Fields1, Synonym) then
            Put_Line
              ("procedure on line " & Lineno & " is for unused synonym");
            raise Done;
         end if;

         Next_Line;

         if not Match (Line, Extr_Field) then
            raise Err;
         end if;

         if Field /= Get (Fields1, Synonym) then
            Put_Line ("Wrong field in procedure Set_" & Synonym);
            raise Done;

         else
            Delete (Fields1, Synonym);
         end if;
      end if;
   end loop;

   Put_Line ("     OK");
   New_Line;
   Put_Line ("Check for missing set procedures");

   declare
      List : constant TV.Table_Array := Convert_To_Array (Fields1);

   begin
      if List'Length > 0 then
         Put_Line ("No procedure for field synonym Set_" & List (1).Name);
         raise Done;
      end if;
   end;

   Put_Line ("     OK");
   New_Line;
   Put_Line ("Check pragma Inlines are all for existing subprograms");

   Clear (Fields1);
   while not End_Of_File (Infil) loop
      Next_Line;

      if Match (Line, Get_Inline)
        and then not Present (Special, Name)
      then
         exit when Match (Name, Set_Name);

         if not Present (Fields, Name) then
            Put_Line
              ("Pragma Inline on line " & Lineno &
               " does not correspond to synonym");
            raise Done;

         else
            Set (Inlines, Name, Get (Inlines, Name) & 'r');
         end if;
      end if;
   end loop;

   Put_Line ("     OK");
   New_Line;
   Put_Line ("Check no pragma Inlines were omitted");

   declare
      List : constant TV.Table_Array := Convert_To_Array (Fields);
      Nxt  : VString := Nul;

   begin
      for M in List'Range loop
         Nxt := List (M).Name;

         if Get (Inlines, Nxt) /= "r" then
            Put_Line ("Incorrect pragma Inlines for " & Nxt);
            raise Done;
         end if;
      end loop;
   end;

   Put_Line ("     OK");
   New_Line;
   Clear (Inlines);

   Close (Infil);
   Open (Infil, In_File, "sinfo.adb");
   Lineno := 0;
   Put_Line ("Check references in functions in body");

   Refscopy := Refs;
   loop
      Next_Line;
      exit when Match (Line, "   -- Field Access Functions --");
   end loop;

   loop
      Next_Line;
      exit when Match (Line, "   -- Field Set Procedures --");

      if Match (Line, Func_Rest)
        and then not Present (Special, Synonym)
      then
         Ref := Get (Refs, Synonym);
         Delete (Refs, Synonym);

         if Ref = "" then
            Put_Line
              ("Function on line " & Lineno & " is for unknown synonym");
            raise Err;
         end if;

         --  Alpha sort of references for this entry

         declare
            Refa   : VStringA (1 .. 100);
            N      : Natural := 0;

         begin
            loop
               exit when not Match (Ref, Get_Nxtref, Nul);
               N := N + 1;
               Refa (N) := Nxtref;
            end loop;

            Sort (Refa (1 .. N));
            Next_Line;
            Next_Line;
            Next_Line;

            --  Checking references for one entry

            for M in 1 .. N loop
               Next_Line;

               if not Match (Line, Test_Syn) then
                  Put_Line ("Expecting N_" & Refa (M) & " at line " & Lineno);
                  raise Done;
               end if;

               Match (Next, Chop_Comma);

               if Next /= Refa (M) then
                  Put_Line ("Expecting N_" & Refa (M) & " at line " & Lineno);
                  raise Done;
               end if;
            end loop;

            Next_Line;
            Match (Line, Return_Fld);

            if Field /= Get (Fields, Synonym) then
               Put_Line
                ("Wrong field for function " & Synonym & " at line " &
                 Lineno & " should be " & Get (Fields, Synonym));
               raise Done;
            end if;
         end;
      end if;
   end loop;

   Put_Line ("     OK");
   New_Line;
   Put_Line ("Check for missing functions in body");

   declare
      List : constant TV.Table_Array := Convert_To_Array (Refs);

   begin
      if List'Length /= 0 then
         Put_Line ("Missing function " & List (1).Name & " in body");
         raise Done;
      end if;
   end;

   Put_Line ("     OK");
   New_Line;
   Put_Line ("Check Set procedures in body");
   Refs := Refscopy;

   loop
      Next_Line;
      exit when Match (Line, "end");
      exit when Match (Line, "   -- Iterator Procedures");

      if Match (Line, Set_Syn)
        and then not Present (Special, Synonym)
      then
         Ref := Get (Refs, Synonym);
         Delete (Refs, Synonym);

         if Ref = "" then
            Put_Line
              ("Function on line " & Lineno & " is for unknown synonym");
            raise Err;
         end if;

         --  Alpha sort of references for this entry

         declare
            Refa   : VStringA (1 .. 100);
            N      : Natural;

         begin
            N := 0;

            loop
               exit when not Match (Ref, Get_Nxtref, Nul);
               N := N + 1;
               Refa (N) := Nxtref;
            end loop;

            Sort (Refa (1 .. N));

            Next_Line;
            Next_Line;
            Next_Line;

            --  Checking references for one entry

            for M in 1 .. N loop
               Next_Line;

               if not Match (Line, Test_Syn)
                 or else Next /= Refa (M)
               then
                  Put_Line ("Expecting N_" & Refa (M) & " at line " & Lineno);
                  raise Err;
               end if;
            end loop;

            loop
               Next_Line;
               exit when Match (Line, Set_Fld);
            end loop;

            Match (Field, Break_With);

            if Field /= Get (Fields, Synonym) then
               Put_Line
                 ("Wrong field for procedure Set_" & Synonym &
                  " at line " & Lineno & " should be " &
                  Get (Fields, Synonym));
               raise Done;
            end if;

            Delete (Fields1, Synonym);
         end;
      end if;
   end loop;

   Put_Line ("     OK");
   New_Line;
   Put_Line ("Check for missing set procedures in body");

   declare
      List : constant TV.Table_Array := Convert_To_Array (Fields1);

   begin
      if List'Length /= 0 then
         Put_Line ("Missing procedure Set_" & List (1).Name & " in body");
         raise Done;
      end if;
   end;

   Put_Line ("     OK");
   New_Line;
   Put_Line ("All tests completed successfully, no errors detected");

exception
   when Done =>
      null;

end CSinfo;
