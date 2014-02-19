------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P A R . S Y N C                              --
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

separate (Par)
package body Sync is

   procedure Resync_Init;
   --  This routine is called on initiating a resynchronization action

   procedure Resync_Resume;
   --  This routine is called on completing a resynchronization action

   -------------------
   -- Resync_Choice --
   -------------------

   procedure Resync_Choice is
   begin
      Resync_Init;

      --  Loop till we get a token that terminates a choice. Note that EOF is
      --  one such token, so we are sure to get out of this loop eventually.

      while Token not in Token_Class_Cterm loop
         Scan;
      end loop;

      Resync_Resume;
   end Resync_Choice;

   ------------------
   -- Resync_Cunit --
   ------------------

   procedure Resync_Cunit is
   begin
      Resync_Init;

      while Token not in Token_Class_Cunit
        and then Token /= Tok_EOF
      loop
         Scan;
      end loop;

      Resync_Resume;
   end Resync_Cunit;

   -----------------------
   -- Resync_Expression --
   -----------------------

   procedure Resync_Expression is
      Paren_Count : Int;

   begin
      Resync_Init;
      Paren_Count := 0;

      loop
         --  Terminating tokens are those in class Eterm and also RANGE,
         --  DIGITS or DELTA if not preceded by an apostrophe (if they are
         --  preceded by an apostrophe, then they are attributes). In addition,
         --  at the outer parentheses level only, we also consider a comma,
         --  right parenthesis or vertical bar to terminate an expression.

         if Token in Token_Class_Eterm

           or else (Token in Token_Class_Atkwd
                     and then Prev_Token /= Tok_Apostrophe)

           or else (Paren_Count = 0
                     and then
                       (Token = Tok_Comma
                         or else Token = Tok_Right_Paren
                         or else Token = Tok_Vertical_Bar))
         then
            --  A special check: if we stop on the ELSE of OR ELSE or the
            --  THEN of AND THEN, keep going, because this is not really an
            --  expression terminator after all. Also, keep going past WITH
            --  since this can be part of an extension aggregate

            if (Token = Tok_Else and then Prev_Token = Tok_Or)
               or else (Token = Tok_Then and then Prev_Token = Tok_And)
               or else Token = Tok_With
            then
               null;
            else
               exit;
            end if;
         end if;

         if Token = Tok_Left_Paren then
            Paren_Count := Paren_Count + 1;

         elsif Token = Tok_Right_Paren then
            Paren_Count := Paren_Count - 1;

         end if;

         Scan; -- past token to be skipped
      end loop;

      Resync_Resume;
   end Resync_Expression;

   -----------------
   -- Resync_Init --
   -----------------

   procedure Resync_Init is
   begin
      --  The following check makes sure we do not get stuck in an infinite
      --  loop resynchronizing and getting nowhere. If we are called to do a
      --  resynchronize and we are exactly at the same point that we left off
      --  on the last resynchronize call, then we force at least one token to
      --  be skipped so that we make progress.

      if Token_Ptr = Last_Resync_Point then
         Scan; -- to skip at least one token
      end if;

      --  Output extra error message if debug R flag is set

      if Debug_Flag_R then
         Error_Msg_SC ("resynchronizing!");
      end if;
   end Resync_Init;

   ----------------------------------
   -- Resync_Past_Malformed_Aspect --
   ----------------------------------

   procedure Resync_Past_Malformed_Aspect is
   begin
      Resync_Init;

      loop
         --  A comma may separate two aspect specifications, but it may also
         --  delimit multiple arguments of a single aspect.

         if Token = Tok_Comma then
            declare
               Scan_State : Saved_Scan_State;

            begin
               Save_Scan_State (Scan_State);
               Scan; -- past comma

               --  The identifier following the comma is a valid aspect, the
               --  current malformed aspect has been successfully skipped.

               if Token = Tok_Identifier
                 and then Get_Aspect_Id (Token_Name) /= No_Aspect
               then
                  Restore_Scan_State (Scan_State);
                  exit;

               --  The comma is delimiting multiple arguments of an aspect

               else
                  Restore_Scan_State (Scan_State);
               end if;
            end;

         --  An IS signals the last aspect specification when the related
         --  context is a body.

         elsif Token = Tok_Is then
            exit;

         --  A semicolon signals the last aspect specification

         elsif Token = Tok_Semicolon then
            exit;

         --  In the case of a mistyped semicolon, any token which follows a
         --  semicolon signals the last aspect specification.

         elsif Token in Token_Class_After_SM then
            exit;
         end if;

         --  Keep on resyncing

         Scan;
      end loop;

      --  Fall out of loop with resynchronization complete

      Resync_Resume;
   end Resync_Past_Malformed_Aspect;

   ---------------------------
   -- Resync_Past_Semicolon --
   ---------------------------

   procedure Resync_Past_Semicolon is
   begin
      Resync_Init;

      loop
         --  Done if we are at a semicolon

         if Token = Tok_Semicolon then
            Scan; -- past semicolon
            exit;

         --  Done if we are at a token which normally appears only after
         --  a semicolon. One special glitch is that the keyword private is
         --  in this category only if it does NOT appear after WITH.

         elsif Token in Token_Class_After_SM
            and then (Token /= Tok_Private or else Prev_Token /= Tok_With)
         then
            exit;

         --  Otherwise keep going

         else
            Scan;
         end if;
      end loop;

      --  Fall out of loop with resynchronization complete

      Resync_Resume;
   end Resync_Past_Semicolon;

   ----------------------------------------------
   -- Resync_Past_Semicolon_Or_To_Loop_Or_Then --
   ----------------------------------------------

   procedure Resync_Past_Semicolon_Or_To_Loop_Or_Then is
   begin
      Resync_Init;

      loop
         --  Done if at semicolon

         if Token = Tok_Semicolon then
            Scan; -- past the semicolon
            exit;

         --  Done if we are at a token which normally appears only after
         --  a semicolon. One special glitch is that the keyword private is
         --  in this category only if it does NOT appear after WITH.

         elsif Token in Token_Class_After_SM
           and then (Token /= Tok_Private or else Prev_Token /= Tok_With)
         then
            exit;

         --  Done if we are at THEN or LOOP

         elsif Token = Tok_Then or else Token = Tok_Loop then
            exit;

         --  Otherwise keep going

         else
            Scan;
         end if;
      end loop;

      --  Fall out of loop with resynchronization complete

      Resync_Resume;
   end Resync_Past_Semicolon_Or_To_Loop_Or_Then;

   -------------------
   -- Resync_Resume --
   -------------------

   procedure Resync_Resume is
   begin
      --  Save resync point (see special test in Resync_Init)

      Last_Resync_Point := Token_Ptr;

      if Debug_Flag_R then
         Error_Msg_SC ("resuming here!");
      end if;
   end Resync_Resume;

   ---------------------------
   -- Resync_Semicolon_List --
   ---------------------------

   procedure Resync_Semicolon_List is
      Paren_Count : Int;

   begin
      Resync_Init;
      Paren_Count := 0;

      loop
         if Token = Tok_EOF
           or else Token = Tok_Semicolon
           or else Token = Tok_Is
           or else Token in Token_Class_After_SM
         then
            exit;

         elsif Token = Tok_Left_Paren then
            Paren_Count := Paren_Count + 1;

         elsif Token = Tok_Right_Paren then
            if Paren_Count = 0 then
               exit;
            else
               Paren_Count := Paren_Count - 1;
            end if;
         end if;

         Scan;
      end loop;

      Resync_Resume;
   end Resync_Semicolon_List;

   -------------------------
   -- Resync_To_Semicolon --
   -------------------------

   procedure Resync_To_Semicolon is
   begin
      Resync_Init;

      loop
         --  Done if we are at a semicolon

         if Token = Tok_Semicolon then
            exit;

         --  Done if we are at a token which normally appears only after
         --  a semicolon. One special glitch is that the keyword private is
         --  in this category only if it does NOT appear after WITH.

         elsif Token in Token_Class_After_SM
           and then (Token /= Tok_Private or else Prev_Token /= Tok_With)
         then
            exit;

         --  Otherwise keep going

         else
            Scan;
         end if;
      end loop;

      --  Fall out of loop with resynchronization complete

      Resync_Resume;
   end Resync_To_Semicolon;

   --------------------
   -- Resync_To_When --
   --------------------

   procedure Resync_To_When is
   begin
      Resync_Init;

      loop
         --  Done if at semicolon, WHEN or IS

         if Token = Tok_Semicolon
           or else Token = Tok_When
           or else Token = Tok_Is
         then
            exit;

         --  Otherwise keep going

         else
            Scan;
         end if;
      end loop;

      --  Fall out of loop with resynchronization complete

      Resync_Resume;
   end Resync_To_When;

end Sync;
