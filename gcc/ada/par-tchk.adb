------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P A R . T C H K                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.1 $
--                                                                          --
--          Copyright (C) 1992-2001 Free Software Foundation, Inc.          --
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

--  Token scan routines.

--  Error recovery: none of the T_xxx or TF_xxx routines raise Error_Resync

separate (Par)
package body Tchk is

   type Position is (SC, BC, AP);
   --  Specify position of error message (see Error_Msg_SC/BC/AP)

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Check_Token (T : Token_Type; P : Position);
   pragma Inline (Check_Token);
   --  Called by T_xx routines to check for reserved keyword token. P is the
   --  position of the error message if the token is missing (see Wrong_Token)

   procedure Wrong_Token (T : Token_Type; P : Position);
   --  Called when scanning a reserved keyword when the keyword is not
   --  present. T is the token type for the keyword, and P indicates the
   --  position to be used to place a message relative to the current
   --  token if the keyword is not located nearby.

   -----------------
   -- Check_Token --
   -----------------

   procedure Check_Token (T : Token_Type; P : Position) is
   begin
      if Token = T then
         Scan;
         return;
      else
         Wrong_Token (T, P);
      end if;
   end Check_Token;

   -------------
   -- T_Abort --
   -------------

   procedure T_Abort is
   begin
      Check_Token (Tok_Abort, SC);
   end T_Abort;

   -------------
   -- T_Arrow --
   -------------

   procedure T_Arrow is
   begin
      if Token = Tok_Arrow then
         Scan;

      --  A little recovery helper, accept then in place of =>

      elsif Token = Tok_Then then
         Error_Msg_BC ("missing ""=>""");
         Scan; -- past THEN used in place of =>

      elsif Token = Tok_Colon_Equal then
         Error_Msg_SC (""":="" should be ""=>""");
         Scan; -- past := used in place of =>

      else
         Error_Msg_AP ("missing ""=>""");
      end if;
   end T_Arrow;

   ----------
   -- T_At --
   ----------

   procedure T_At is
   begin
      Check_Token (Tok_At, SC);
   end T_At;

   ------------
   -- T_Body --
   ------------

   procedure T_Body is
   begin
      Check_Token (Tok_Body, BC);
   end T_Body;

   -----------
   -- T_Box --
   -----------

   procedure T_Box is
   begin
      if Token = Tok_Box then
         Scan;
      else
         Error_Msg_AP ("missing ""<>""");
      end if;
   end T_Box;

   -------------
   -- T_Colon --
   -------------

   procedure T_Colon is
   begin
      if Token = Tok_Colon then
         Scan;
      else
         Error_Msg_AP ("missing "":""");
      end if;
   end T_Colon;

   -------------------
   -- T_Colon_Equal --
   -------------------

   procedure T_Colon_Equal is
   begin
      if Token = Tok_Colon_Equal then
         Scan;

      elsif Token = Tok_Equal then
         Error_Msg_SC ("""="" should be "":=""");
         Scan;

      elsif Token = Tok_Colon then
         Error_Msg_SC (""":"" should be "":=""");
         Scan;

      elsif Token = Tok_Is then
         Error_Msg_SC ("IS should be "":=""");
         Scan;

      else
         Error_Msg_AP ("missing "":=""");
      end if;
   end T_Colon_Equal;

   -------------
   -- T_Comma --
   -------------

   procedure T_Comma is
   begin
      if Token = Tok_Comma then
         Scan;

      else
         if Token = Tok_Pragma then
            P_Pragmas_Misplaced;
         end if;

         if Token = Tok_Comma then
            Scan;
         else
            Error_Msg_AP ("missing "",""");
         end if;
      end if;

      if Token = Tok_Pragma then
         P_Pragmas_Misplaced;
      end if;
   end T_Comma;

   ---------------
   -- T_Dot_Dot --
   ---------------

   procedure T_Dot_Dot is
   begin
      if Token = Tok_Dot_Dot then
         Scan;
      else
         Error_Msg_AP ("missing ""..""");
      end if;
   end T_Dot_Dot;

   -----------
   -- T_For --
   -----------

   procedure T_For is
   begin
      Check_Token (Tok_For, AP);
   end T_For;

   -----------------------
   -- T_Greater_Greater --
   -----------------------

   procedure T_Greater_Greater is
   begin
      if Token = Tok_Greater_Greater then
         Scan;
      else
         Error_Msg_AP ("missing "">>""");
      end if;
   end T_Greater_Greater;

   ------------------
   -- T_Identifier --
   ------------------

   procedure T_Identifier is
   begin
      if Token = Tok_Identifier then
         Scan;
      elsif Token in Token_Class_Literal then
         Error_Msg_SC ("identifier expected");
         Scan;
      else
         Error_Msg_AP ("identifier expected");
      end if;
   end T_Identifier;

   ----------
   -- T_In --
   ----------

   procedure T_In is
   begin
      Check_Token (Tok_In, AP);
   end T_In;

   ----------
   -- T_Is --
   ----------

   procedure T_Is is
   begin
      if Token = Tok_Is then
         Scan;

         Ignore (Tok_Semicolon);

      --  Allow OF, => or = to substitute for IS with complaint

      elsif Token = Tok_Arrow
        or else Token = Tok_Of
        or else Token = Tok_Equal
      then
         Error_Msg_SC ("missing IS");
         Scan; -- token used in place of IS
      else
         Wrong_Token (Tok_Is, AP);
      end if;

      while Token = Tok_Is loop
         Error_Msg_SC ("extra IS ignored");
         Scan;
      end loop;
   end T_Is;

   ------------------
   -- T_Left_Paren --
   ------------------

   procedure T_Left_Paren is
   begin
      if Token = Tok_Left_Paren then
         Scan;
      else
         Error_Msg_AP ("missing ""(""");
      end if;
   end T_Left_Paren;

   ------------
   -- T_Loop --
   ------------

   procedure T_Loop is
   begin
      if Token = Tok_Do then
         Error_Msg_SC ("LOOP expected");
         Scan;
      else
         Check_Token (Tok_Loop, AP);
      end if;
   end T_Loop;

   -----------
   -- T_Mod --
   -----------

   procedure T_Mod is
   begin
      Check_Token (Tok_Mod, AP);
   end T_Mod;

   -----------
   -- T_New --
   -----------

   procedure T_New is
   begin
      Check_Token (Tok_New, AP);
   end T_New;

   ----------
   -- T_Of --
   ----------

   procedure T_Of is
   begin
      Check_Token (Tok_Of, AP);
   end T_Of;

   ----------
   -- T_Or --
   ----------

   procedure T_Or is
   begin
      Check_Token (Tok_Or, AP);
   end T_Or;

   ---------------
   -- T_Private --
   ---------------

   procedure T_Private is
   begin
      Check_Token (Tok_Private, SC);
   end T_Private;

   -------------
   -- T_Range --
   -------------

   procedure T_Range is
   begin
      Check_Token (Tok_Range, AP);
   end T_Range;

   --------------
   -- T_Record --
   --------------

   procedure T_Record is
   begin
      Check_Token (Tok_Record, AP);
   end T_Record;

   -------------------
   -- T_Right_Paren --
   -------------------

   procedure T_Right_Paren is
   begin
      if Token = Tok_Right_Paren then
         Scan;
      else
         Error_Msg_AP ("missing "")""");
      end if;
   end T_Right_Paren;

   -----------------
   -- T_Semicolon --
   -----------------

   procedure T_Semicolon is
   begin

      if Token = Tok_Semicolon then
         Scan;

         if Token = Tok_Semicolon then
            Error_Msg_SC ("extra "";"" ignored");
            Scan;
         end if;

      elsif Token = Tok_Colon then
         Error_Msg_SC (""":"" should be "";""");
         Scan;

      elsif Token = Tok_Comma then
         Error_Msg_SC (""","" should be "";""");
         Scan;

      elsif Token = Tok_Dot then
         Error_Msg_SC ("""."" should be "";""");
         Scan;

      --  An interesting little kludge here. If the previous token is a
      --  semicolon, then there is no way that we can legitimately need
      --  another semicolon. This could only arise in an error situation
      --  where an error has already been signalled. By simply ignoring
      --  the request for a semicolon in this case, we avoid some spurious
      --  missing semicolon messages.

      elsif Prev_Token = Tok_Semicolon then
         return;

      --  If the current token is | then this is a reasonable
      --  place to suggest the possibility of a "C" confusion :-)

      elsif Token = Tok_Vertical_Bar then
         Error_Msg_SC ("unexpected occurrence of ""|"", did you mean OR'?");
         Resync_Past_Semicolon;

      --  Otherwise we really do have a missing semicolon

      else
         Error_Msg_AP ("missing "";""");
         return;
      end if;

   end T_Semicolon;

   ------------
   -- T_Then --
   ------------

   procedure T_Then is
   begin
      Check_Token (Tok_Then, AP);
   end T_Then;

   ------------
   -- T_Type --
   ------------

   procedure T_Type is
   begin
      Check_Token (Tok_Type, BC);
   end T_Type;

   -----------
   -- T_Use --
   -----------

   procedure T_Use is
   begin
      Check_Token (Tok_Use, SC);
   end T_Use;

   ------------
   -- T_When --
   ------------

   procedure T_When is
   begin
      Check_Token (Tok_When, SC);
   end T_When;

   ------------
   -- T_With --
   ------------

   procedure T_With is
   begin
      Check_Token (Tok_With, BC);
   end T_With;

   --------------
   -- TF_Arrow --
   --------------

   procedure TF_Arrow is
      Scan_State : Saved_Scan_State;

   begin
      if Token = Tok_Arrow then
         Scan; -- skip arrow and we are done

      elsif Token = Tok_Colon_Equal then
         T_Arrow; -- Let T_Arrow give the message

      else
         T_Arrow; -- give missing arrow message
         Save_Scan_State (Scan_State); -- at start of junk tokens

         loop
            if Prev_Token_Ptr < Current_Line_Start
              or else Token = Tok_Semicolon
              or else Token = Tok_EOF
            then
               Restore_Scan_State (Scan_State); -- to where we were!
               return;
            end if;

            Scan; -- continue search!

            if Token = Tok_Arrow then
               Scan; -- past arrow
               return;
            end if;
         end loop;
      end if;
   end TF_Arrow;

   -----------
   -- TF_Is --
   -----------

   procedure TF_Is is
      Scan_State : Saved_Scan_State;

   begin
      if Token = Tok_Is then
         T_Is; -- past IS and we are done

      --  Allow OF or => or = in place of IS (with error message)

      elsif Token = Tok_Of
        or else Token = Tok_Arrow
        or else Token = Tok_Equal
      then
         T_Is; -- give missing IS message and skip bad token

      else
         T_Is; -- give missing IS message
         Save_Scan_State (Scan_State); -- at start of junk tokens

         loop
            if Prev_Token_Ptr < Current_Line_Start
              or else Token = Tok_Semicolon
              or else Token = Tok_EOF
            then
               Restore_Scan_State (Scan_State); -- to where we were!
               return;
            end if;

            Scan; -- continue search!

            if Token = Tok_Is
              or else Token = Tok_Of
              or else Token = Tok_Arrow
            then
               Scan; -- past IS or OF or =>
               return;
            end if;
         end loop;
      end if;
   end TF_Is;

   -------------
   -- TF_Loop --
   -------------

   procedure TF_Loop is
      Scan_State : Saved_Scan_State;

   begin
      if Token = Tok_Loop then
         Scan; -- past LOOP and we are done

      --  Allow DO or THEN in place of LOOP

      elsif Token = Tok_Then or else Token = Tok_Do then
         T_Loop; -- give missing LOOP message

      else
         T_Loop; -- give missing LOOP message
         Save_Scan_State (Scan_State); -- at start of junk tokens

         loop
            if Prev_Token_Ptr < Current_Line_Start
              or else Token = Tok_Semicolon
              or else Token = Tok_EOF
            then
               Restore_Scan_State (Scan_State); -- to where we were!
               return;
            end if;

            Scan; -- continue search!

            if Token = Tok_Loop or else Token = Tok_Then then
               Scan; -- past loop or then (message already generated)
               return;
            end if;
         end loop;
      end if;
   end TF_Loop;

   --------------
   -- TF_Return--
   --------------

   procedure TF_Return is
      Scan_State : Saved_Scan_State;

   begin
      if Token = Tok_Return then
         Scan; -- skip RETURN and we are done

      else
         Error_Msg_SC ("missing RETURN");
         Save_Scan_State (Scan_State); -- at start of junk tokens

         loop
            if Prev_Token_Ptr < Current_Line_Start
              or else Token = Tok_Semicolon
              or else Token = Tok_EOF
            then
               Restore_Scan_State (Scan_State); -- to where we were!
               return;
            end if;

            Scan; -- continue search!

            if Token = Tok_Return then
               Scan; -- past RETURN
               return;
            end if;
         end loop;
      end if;
   end TF_Return;

   ------------------
   -- TF_Semicolon --
   ------------------

   procedure TF_Semicolon is
      Scan_State : Saved_Scan_State;

   begin
      if Token = Tok_Semicolon then
         T_Semicolon;
         return;

      --  An interesting little kludge here. If the previous token is a
      --  semicolon, then there is no way that we can legitimately need
      --  another semicolon. This could only arise in an error situation
      --  where an error has already been signalled. By simply ignoring
      --  the request for a semicolon in this case, we avoid some spurious
      --  missing semicolon messages.

      elsif Prev_Token = Tok_Semicolon then
         return;

      else
         if Token = Tok_Pragma then
            P_Pragmas_Misplaced;

            if Token = Tok_Semicolon then
               T_Semicolon;
               return;
            end if;
         end if;

         T_Semicolon; -- give missing semicolon message
         Save_Scan_State (Scan_State); -- at start of junk tokens

         loop
            if Prev_Token_Ptr < Current_Line_Start
              or else Token = Tok_EOF
            then
               Restore_Scan_State (Scan_State); -- to where we were
               return;
            end if;

            Scan; -- continue search

            if Token = Tok_Semicolon then
               T_Semicolon;
               return;

            elsif Token in Token_Class_After_SM then
               return;
            end if;
         end loop;
      end if;
   end TF_Semicolon;

   -------------
   -- TF_Then --
   -------------

   procedure TF_Then is
      Scan_State : Saved_Scan_State;

   begin
      if Token = Tok_Then then
         Scan; -- past THEN and we are done

      else
         T_Then; -- give missing THEN message
         Save_Scan_State (Scan_State); -- at start of junk tokens

         loop
            if Prev_Token_Ptr < Current_Line_Start
              or else Token = Tok_Semicolon
              or else Token = Tok_EOF
            then
               Restore_Scan_State (Scan_State); -- to where we were
               return;
            end if;

            Scan; -- continue search!

            if Token = Tok_Then then
               Scan; -- past THEN
               return;
            end if;
         end loop;
      end if;
   end TF_Then;

   ------------
   -- TF_Use --
   ------------

   procedure TF_Use is
      Scan_State : Saved_Scan_State;

   begin
      if Token = Tok_Use then
         Scan; -- past USE and we are done

      else
         T_Use; -- give USE expected message
         Save_Scan_State (Scan_State); -- at start of junk tokens

         loop
            if Prev_Token_Ptr < Current_Line_Start
              or else Token = Tok_Semicolon
              or else Token = Tok_EOF
            then
               Restore_Scan_State (Scan_State); -- to where we were
               return;
            end if;

            Scan; -- continue search!

            if Token = Tok_Use then
               Scan; -- past use
               return;
            end if;
         end loop;
      end if;
   end TF_Use;

   -----------------
   -- Wrong_Token --
   -----------------

   procedure Wrong_Token (T : Token_Type; P : Position) is
      Missing : constant String := "missing ";
      Image : constant String := Token_Type'Image (T);
      Tok_Name : constant String := Image (5 .. Image'Length);
      M : String (1 .. Missing'Length + Tok_Name'Length);

   begin
      --  Set M to Missing & Tok_Name.

      M (1 .. Missing'Length) := Missing;
      M (Missing'Length + 1 .. M'Last) := Tok_Name;

      if Token = Tok_Semicolon then
         Scan;

         if Token = T then
            Error_Msg_SP ("extra "";"" ignored");
            Scan;
         else
            Error_Msg_SP (M);
         end if;

      elsif Token = Tok_Comma then
         Scan;

         if Token = T then
            Error_Msg_SP ("extra "","" ignored");
            Scan;

         else
            Error_Msg_SP (M);
         end if;

      else
         case P is
            when SC => Error_Msg_SC (M);
            when BC => Error_Msg_BC (M);
            when AP => Error_Msg_AP (M);
         end case;
      end if;
   end Wrong_Token;

end Tchk;
