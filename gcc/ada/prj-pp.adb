------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               P R J . P P                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2001-2004 Free Software Foundation, Inc.       --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Handling; use Ada.Characters.Handling;

with Hostparm;
with Namet;    use Namet;
with Output;   use Output;
with Snames;

package body Prj.PP is

   use Prj.Tree;

   Not_Tested : array (Project_Node_Kind) of Boolean := (others => True);

   Max_Line_Length : constant := Hostparm.Max_Line_Length - 5;
   --  Maximum length of a line.

   Column : Natural := 0;
   --  Column number of the last character in the line. Used to avoid
   --  outputing lines longer than Max_Line_Length.

   procedure Indicate_Tested (Kind : Project_Node_Kind);
   --  Set the corresponding component of array Not_Tested to False.
   --  Only called by pragmas Debug.

   ---------------------
   -- Indicate_Tested --
   ---------------------

   procedure Indicate_Tested (Kind : Project_Node_Kind) is
   begin
      Not_Tested (Kind) := False;
   end Indicate_Tested;

   ------------------
   -- Pretty_Print --
   ------------------

   procedure Pretty_Print
     (Project                            : Prj.Tree.Project_Node_Id;
      Increment                          : Positive      := 3;
      Eliminate_Empty_Case_Constructions : Boolean       := False;
      Minimize_Empty_Lines               : Boolean       := False;
      W_Char                             : Write_Char_Ap := null;
      W_Eol                              : Write_Eol_Ap  := null;
      W_Str                              : Write_Str_Ap  := null;
      Backward_Compatibility             : Boolean)
   is
      procedure Print (Node : Project_Node_Id; Indent : Natural);
      --  A recursive procedure that traverses a project file tree and outputs
      --  its source. Current_Prj is the project that we are printing. This
      --  is used when printing attributes, since in nested packages they
      --  need to use a fully qualified name.

      procedure Output_Attribute_Name (Name : Name_Id);
      --  Outputs an attribute name, taking into account the value of
      --  Backward_Compatibility.

      procedure Output_Name (Name : Name_Id; Capitalize : Boolean := True);
      --  Outputs a name

      procedure Start_Line (Indent : Natural);
      --  Outputs the indentation at the beginning of the line.

      procedure Output_String (S : Name_Id);
      --  Outputs a string using the default output procedures

      procedure Write_Empty_Line (Always : Boolean := False);
      --  Outputs an empty line, only if the previous line was not empty
      --  already and either Always is True or Minimize_Empty_Lines is False.

      procedure Write_Line (S : String);
      --  Outputs S followed by a new line

      procedure Write_String (S : String; Truncated : Boolean := False);
      --  Outputs S using Write_Str, starting a new line if line would
      --  become too long, when Truncated = False.
      --  When Truncated = True, only the part of the string that can fit on
      --  the line is output.

      procedure Write_End_Of_Line_Comment (Node : Project_Node_Id);

      Write_Char : Write_Char_Ap := Output.Write_Char'Access;
      Write_Eol  : Write_Eol_Ap  := Output.Write_Eol'Access;
      Write_Str  : Write_Str_Ap  := Output.Write_Str'Access;
      --  These three access to procedure values are used for the output.

      Last_Line_Is_Empty : Boolean := False;
      --  Used to avoid two consecutive empty lines.

      ---------------------------
      -- Output_Attribute_Name --
      ---------------------------

      procedure Output_Attribute_Name (Name : Name_Id) is
      begin
         if Backward_Compatibility then
            case Name is
               when Snames.Name_Spec =>
                  Output_Name (Snames.Name_Specification);

               when Snames.Name_Spec_Suffix =>
                  Output_Name (Snames.Name_Specification_Suffix);

               when Snames.Name_Body =>
                  Output_Name (Snames.Name_Implementation);

               when Snames.Name_Body_Suffix =>
                  Output_Name (Snames.Name_Implementation_Suffix);

               when others =>
                  Output_Name (Name);
            end case;

         else
            Output_Name (Name);
         end if;
      end Output_Attribute_Name;

      -----------------
      -- Output_Name --
      -----------------

      procedure Output_Name (Name : Name_Id; Capitalize : Boolean := True) is
         Capital : Boolean := Capitalize;

      begin
         Get_Name_String (Name);

         --  If line would become too long, create new line

         if Column + Name_Len > Max_Line_Length then
            Write_Eol.all;
            Column := 0;
         end if;

         for J in 1 .. Name_Len loop
            if Capital then
               Write_Char (To_Upper (Name_Buffer (J)));
            else
               Write_Char (Name_Buffer (J));
            end if;

            if Capitalize then
               Capital :=
                 Name_Buffer (J) = '_'
                 or else Is_Digit (Name_Buffer (J));
            end if;
         end loop;

         Column := Column + Name_Len;
      end Output_Name;

      -------------------
      -- Output_String --
      -------------------

      procedure Output_String (S : Name_Id) is
      begin
         Get_Name_String (S);

         --  If line could become too long, create new line.
         --  Note that the number of characters on the line could be
         --  twice the number of character in the string (if every
         --  character is a '"') plus two (the initial and final '"').

         if Column + Name_Len + Name_Len + 2 > Max_Line_Length then
            Write_Eol.all;
            Column := 0;
         end if;

         Write_Char ('"');
         Column := Column + 1;
         Get_Name_String (S);

         for J in 1 .. Name_Len loop
            if Name_Buffer (J) = '"' then
               Write_Char ('"');
               Write_Char ('"');
               Column := Column + 2;
            else
               Write_Char (Name_Buffer (J));
               Column := Column + 1;
            end if;

            --  If the string does not fit on one line, cut it in parts
            --  and concatenate.

            if J < Name_Len and then Column >= Max_Line_Length then
               Write_Str (""" &");
               Write_Eol.all;
               Write_Char ('"');
               Column := 1;
            end if;
         end loop;

         Write_Char ('"');
         Column := Column + 1;
      end Output_String;

      ----------------
      -- Start_Line --
      ----------------

      procedure Start_Line (Indent : Natural) is
      begin
         if not Minimize_Empty_Lines then
            Write_Str ((1 .. Indent => ' '));
            Column := Column + Indent;
         end if;
      end Start_Line;

      ----------------------
      -- Write_Empty_Line --
      ----------------------

      procedure Write_Empty_Line (Always : Boolean := False) is
      begin
         if (Always or else not Minimize_Empty_Lines)
           and then not Last_Line_Is_Empty then
            Write_Eol.all;
            Column := 0;
            Last_Line_Is_Empty := True;
         end if;
      end Write_Empty_Line;

      -------------------------------
      -- Write_End_Of_Line_Comment --
      -------------------------------

      procedure Write_End_Of_Line_Comment (Node : Project_Node_Id) is
         Value : constant Name_Id := End_Of_Line_Comment (Node);

      begin
         if Value /= No_Name then
            Write_String (" --");
            Write_String (Get_Name_String (Value), Truncated => True);
         end if;

         Write_Line ("");
      end Write_End_Of_Line_Comment;

      ----------------
      -- Write_Line --
      ----------------

      procedure Write_Line (S : String) is
      begin
         Write_String (S);
         Last_Line_Is_Empty := False;
         Write_Eol.all;
         Column := 0;
      end Write_Line;

      ------------------
      -- Write_String --
      ------------------

      procedure Write_String (S : String; Truncated : Boolean := False) is
         Length : Natural := S'Length;
      begin
         --  If the string would not fit on the line,
         --  start a new line.

         if Column + Length > Max_Line_Length then
            if Truncated then
               Length := Max_Line_Length - Column;

            else
               Write_Eol.all;
               Column := 0;
            end if;
         end if;

         Write_Str (S (S'First .. S'First + Length - 1));
         Column := Column + Length;
      end Write_String;

      -----------
      -- Print --
      -----------

      procedure Print (Node   : Project_Node_Id; Indent : Natural) is
      begin
         if Node /= Empty_Node then

            case Kind_Of (Node) is

               when N_Project  =>
                  pragma Debug (Indicate_Tested (N_Project));
                  if First_With_Clause_Of (Node) /= Empty_Node then

                     --  with clause(s)

                     Print (First_With_Clause_Of (Node), Indent);
                     Write_Empty_Line (Always => True);
                  end if;

                  Print (First_Comment_Before (Node), Indent);
                  Start_Line (Indent);
                  Write_String ("project ");
                  Output_Name (Name_Of (Node));

                  --  Check if this project extends another project

                  if Extended_Project_Path_Of (Node) /= No_Name then
                     Write_String (" extends ");
                     Output_String (Extended_Project_Path_Of (Node));
                  end if;

                  Write_String (" is");
                  Write_End_Of_Line_Comment (Node);
                  Print (First_Comment_After (Node), Indent + Increment);
                  Write_Empty_Line (Always => True);

                  --  Output all of the declarations in the project

                  Print (Project_Declaration_Of (Node), Indent);
                  Print (First_Comment_Before_End (Node), Indent + Increment);
                  Start_Line (Indent);
                  Write_String ("end ");
                  Output_Name (Name_Of (Node));
                  Write_Line (";");
                  Print (First_Comment_After_End (Node), Indent);

               when N_With_Clause =>
                  pragma Debug (Indicate_Tested (N_With_Clause));

                  if Name_Of (Node) /= No_Name then
                     Print (First_Comment_Before (Node), Indent);
                     Start_Line (Indent);

                     if Non_Limited_Project_Node_Of (Node) = Empty_Node then
                        Write_String ("limited ");
                     end if;

                     Write_String ("with ");
                     Output_String (String_Value_Of (Node));
                     Write_String (";");
                     Write_End_Of_Line_Comment (Node);
                     Print (First_Comment_After (Node), Indent);
                  end if;

                  Print (Next_With_Clause_Of (Node), Indent);

               when N_Project_Declaration =>
                  pragma Debug (Indicate_Tested (N_Project_Declaration));

                  if First_Declarative_Item_Of (Node) /= Empty_Node then
                     Print
                       (First_Declarative_Item_Of (Node), Indent + Increment);
                     Write_Empty_Line (Always => True);
                  end if;

               when N_Declarative_Item =>
                  pragma Debug (Indicate_Tested (N_Declarative_Item));
                  Print (Current_Item_Node (Node), Indent);
                  Print (Next_Declarative_Item (Node), Indent);

               when N_Package_Declaration =>
                  pragma Debug (Indicate_Tested (N_Package_Declaration));
                  Write_Empty_Line (Always => True);
                  Print (First_Comment_Before (Node), Indent);
                  Start_Line (Indent);
                  Write_String ("package ");
                  Output_Name (Name_Of (Node));

                  if Project_Of_Renamed_Package_Of (Node) /= Empty_Node then
                     Write_String (" renames ");
                     Output_Name
                       (Name_Of (Project_Of_Renamed_Package_Of (Node)));
                     Write_String (".");
                     Output_Name (Name_Of (Node));
                     Write_String (";");
                     Write_End_Of_Line_Comment (Node);
                     Print (First_Comment_After_End (Node), Indent);

                  else
                     Write_String (" is");
                     Write_End_Of_Line_Comment (Node);
                     Print (First_Comment_After (Node), Indent + Increment);

                     if First_Declarative_Item_Of (Node) /= Empty_Node then
                        Print
                          (First_Declarative_Item_Of (Node),
                           Indent + Increment);
                     end if;

                     Print (First_Comment_Before_End (Node),
                            Indent + Increment);
                     Start_Line (Indent);
                     Write_String ("end ");
                     Output_Name (Name_Of (Node));
                     Write_Line (";");
                     Print (First_Comment_After_End (Node), Indent);
                     Write_Empty_Line;
                  end if;

               when N_String_Type_Declaration =>
                  pragma Debug (Indicate_Tested (N_String_Type_Declaration));
                  Print (First_Comment_Before (Node), Indent);
                  Start_Line (Indent);
                  Write_String ("type ");
                  Output_Name (Name_Of (Node));
                  Write_Line (" is");
                  Start_Line (Indent + Increment);
                  Write_String ("(");

                  declare
                     String_Node : Project_Node_Id :=
                       First_Literal_String (Node);

                  begin
                     while String_Node /= Empty_Node loop
                        Output_String (String_Value_Of (String_Node));
                        String_Node := Next_Literal_String (String_Node);

                        if String_Node /= Empty_Node then
                           Write_String (", ");
                        end if;
                     end loop;
                  end;

                  Write_String (");");
                  Write_End_Of_Line_Comment (Node);
                  Print (First_Comment_After (Node), Indent);

               when N_Literal_String =>
                  pragma Debug (Indicate_Tested (N_Literal_String));
                  Output_String (String_Value_Of (Node));

                  if Source_Index_Of (Node) /= 0 then
                     Write_String (" at ");
                     Write_String (Source_Index_Of (Node)'Img);
                  end if;

               when N_Attribute_Declaration =>
                  pragma Debug (Indicate_Tested (N_Attribute_Declaration));
                  Print (First_Comment_Before (Node), Indent);
                  Start_Line (Indent);
                  Write_String ("for ");
                  Output_Attribute_Name (Name_Of (Node));

                  if Associative_Array_Index_Of (Node) /= No_Name then
                     Write_String (" (");
                     Output_String (Associative_Array_Index_Of (Node));

                     if Source_Index_Of (Node) /= 0 then
                        Write_String (" at ");
                        Write_String (Source_Index_Of (Node)'Img);
                     end if;

                     Write_String (")");
                  end if;

                  Write_String (" use ");
                  Print (Expression_Of (Node), Indent);
                  Write_String (";");
                  Write_End_Of_Line_Comment (Node);
                  Print (First_Comment_After (Node), Indent);

               when N_Typed_Variable_Declaration =>
                  pragma Debug
                    (Indicate_Tested (N_Typed_Variable_Declaration));
                  Print (First_Comment_Before (Node), Indent);
                  Start_Line (Indent);
                  Output_Name (Name_Of (Node));
                  Write_String (" : ");
                  Output_Name (Name_Of (String_Type_Of (Node)));
                  Write_String (" := ");
                  Print (Expression_Of (Node), Indent);
                  Write_String (";");
                  Write_End_Of_Line_Comment (Node);
                  Print (First_Comment_After (Node), Indent);

               when N_Variable_Declaration =>
                  pragma Debug (Indicate_Tested (N_Variable_Declaration));
                  Print (First_Comment_Before (Node), Indent);
                  Start_Line (Indent);
                  Output_Name (Name_Of (Node));
                  Write_String (" := ");
                  Print (Expression_Of (Node), Indent);
                  Write_String (";");
                  Write_End_Of_Line_Comment (Node);
                  Print (First_Comment_After (Node), Indent);

               when N_Expression =>
                  pragma Debug (Indicate_Tested (N_Expression));
                  declare
                     Term : Project_Node_Id := First_Term (Node);

                  begin
                     while Term /= Empty_Node loop
                        Print (Term, Indent);
                        Term := Next_Term (Term);

                        if Term /= Empty_Node then
                           Write_String (" & ");
                        end if;
                     end loop;
                  end;

               when N_Term =>
                  pragma Debug (Indicate_Tested (N_Term));
                  Print (Current_Term (Node), Indent);

               when N_Literal_String_List =>
                  pragma Debug (Indicate_Tested (N_Literal_String_List));
                  Write_String ("(");

                  declare
                     Expression : Project_Node_Id :=
                       First_Expression_In_List (Node);

                  begin
                     while Expression /= Empty_Node loop
                        Print (Expression, Indent);
                        Expression := Next_Expression_In_List (Expression);

                        if Expression /= Empty_Node then
                           Write_String (", ");
                        end if;
                     end loop;
                  end;

                  Write_String (")");

               when N_Variable_Reference =>
                  pragma Debug (Indicate_Tested (N_Variable_Reference));
                  if Project_Node_Of (Node) /= Empty_Node then
                     Output_Name (Name_Of (Project_Node_Of (Node)));
                     Write_String (".");
                  end if;

                  if Package_Node_Of (Node) /= Empty_Node then
                     Output_Name (Name_Of (Package_Node_Of (Node)));
                     Write_String (".");
                  end if;

                  Output_Name (Name_Of (Node));

               when N_External_Value =>
                  pragma Debug (Indicate_Tested (N_External_Value));
                  Write_String ("external (");
                  Print (External_Reference_Of (Node), Indent);

                  if External_Default_Of (Node) /= Empty_Node then
                     Write_String (", ");
                     Print (External_Default_Of (Node), Indent);
                  end if;

                  Write_String (")");

               when N_Attribute_Reference =>
                  pragma Debug (Indicate_Tested (N_Attribute_Reference));

                  if Project_Node_Of (Node) /= Empty_Node
                    and then Project_Node_Of (Node) /= Project
                  then
                     Output_Name (Name_Of (Project_Node_Of (Node)));

                     if Package_Node_Of (Node) /= Empty_Node then
                        Write_String (".");
                        Output_Name (Name_Of (Package_Node_Of (Node)));
                     end if;

                  elsif Package_Node_Of (Node) /= Empty_Node then
                     Output_Name (Name_Of (Package_Node_Of (Node)));

                  else
                     Write_String ("project");
                  end if;

                  Write_String ("'");
                  Output_Attribute_Name (Name_Of (Node));

                  declare
                     Index : constant Name_Id :=
                       Associative_Array_Index_Of (Node);

                  begin
                     if Index /= No_Name then
                        Write_String (" (");
                        Output_String (Index);
                        Write_String (")");
                     end if;
                  end;

               when N_Case_Construction =>
                  pragma Debug (Indicate_Tested (N_Case_Construction));

                  declare
                     Case_Item : Project_Node_Id := First_Case_Item_Of (Node);
                     Is_Non_Empty : Boolean := False;
                  begin
                     while Case_Item /= Empty_Node loop
                        if First_Declarative_Item_Of (Case_Item) /= Empty_Node
                          or else not Eliminate_Empty_Case_Constructions
                        then
                           Is_Non_Empty := True;
                           exit;
                        end if;
                        Case_Item := Next_Case_Item (Case_Item);
                     end loop;

                     if Is_Non_Empty then
                        Write_Empty_Line;
                        Print (First_Comment_Before (Node), Indent);
                        Start_Line (Indent);
                        Write_String ("case ");
                        Print (Case_Variable_Reference_Of (Node), Indent);
                        Write_String (" is");
                        Write_End_Of_Line_Comment (Node);
                        Print (First_Comment_After (Node), Indent + Increment);

                        declare
                           Case_Item : Project_Node_Id :=
                             First_Case_Item_Of (Node);

                        begin
                           while Case_Item /= Empty_Node loop
                              pragma Assert
                                (Kind_Of (Case_Item) = N_Case_Item);
                              Print (Case_Item, Indent + Increment);
                              Case_Item := Next_Case_Item (Case_Item);
                           end loop;
                        end;

                        Print (First_Comment_Before_End (Node),
                               Indent + Increment);
                        Start_Line (Indent);
                        Write_Line ("end case;");
                        Print (First_Comment_After_End (Node), Indent);
                     end if;
                  end;

               when N_Case_Item =>
                  pragma Debug (Indicate_Tested (N_Case_Item));

                  if First_Declarative_Item_Of (Node) /= Empty_Node
                    or else not Eliminate_Empty_Case_Constructions
                  then
                     Write_Empty_Line;
                     Print (First_Comment_Before (Node), Indent);
                     Start_Line (Indent);
                     Write_String ("when ");

                     if First_Choice_Of (Node) = Empty_Node then
                        Write_String ("others");

                     else
                        declare
                           Label : Project_Node_Id := First_Choice_Of (Node);

                        begin
                           while Label /= Empty_Node loop
                              Print (Label, Indent);
                              Label := Next_Literal_String (Label);

                              if Label /= Empty_Node then
                                 Write_String (" | ");
                              end if;
                           end loop;
                        end;
                     end if;

                     Write_String (" =>");
                     Write_End_Of_Line_Comment (Node);
                     Print (First_Comment_After (Node), Indent + Increment);

                     declare
                        First : constant Project_Node_Id :=
                                  First_Declarative_Item_Of (Node);

                     begin
                        if First = Empty_Node then
                           Write_Empty_Line;

                        else
                           Print (First, Indent + Increment);
                        end if;
                     end;
                  end if;

               when N_Comment_Zones =>

               --  Nothing to do, because it will not be processed directly

                  null;

               when N_Comment =>
                  pragma Debug (Indicate_Tested (N_Comment));

                  if Follows_Empty_Line (Node) then
                     Write_Empty_Line;
                  end if;

                  Start_Line (Indent);
                  Write_String ("--");
                  Write_String
                    (Get_Name_String (String_Value_Of (Node)),
                     Truncated => True);
                  Write_Line ("");

                  if Is_Followed_By_Empty_Line (Node) then
                     Write_Empty_Line;
                  end if;

                  Print (Next_Comment (Node), Indent);
            end case;
         end if;
      end Print;

   --  Start of processing for Pretty_Print

   begin
      if W_Char = null then
         Write_Char := Output.Write_Char'Access;
      else
         Write_Char := W_Char;
      end if;

      if W_Eol = null then
         Write_Eol := Output.Write_Eol'Access;
      else
         Write_Eol := W_Eol;
      end if;

      if W_Str = null then
         Write_Str := Output.Write_Str'Access;
      else
         Write_Str := W_Str;
      end if;

      Print (Project, 0);

      if W_Char = null or else W_Str = null then
         Output.Write_Eol;
      end if;
   end Pretty_Print;

   -----------------------
   -- Output_Statistics --
   -----------------------

   procedure Output_Statistics is
   begin
      Output.Write_Line ("Project_Node_Kinds not tested:");

      for Kind in Project_Node_Kind loop
         if Kind /= N_Comment_Zones and then Not_Tested (Kind) then
            Output.Write_Str ("   ");
            Output.Write_Line (Project_Node_Kind'Image (Kind));
         end if;
      end loop;

      Output.Write_Eol;
   end Output_Statistics;

end Prj.PP;
