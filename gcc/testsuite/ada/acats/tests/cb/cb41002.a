-- CB41002.A
--
--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
--     F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained
--     unlimited rights in the software and documentation contained herein.
--     Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making
--     this public release, the Government intends to confer upon all
--     recipients unlimited rights  equal to those held by the Government.
--     These rights include rights to use, duplicate, release or disclose the
--     released technical data and computer software in whole or in part, in
--     any manner and for any purpose whatsoever, and to have or permit others
--     to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
--
-- OBJECTIVE:
--      Check that the message string input parameter in a call to the
--      Raise_Exception procedure is associated with the raised exception
--      occurrence, and that the message string can be obtained using the
--      Exception_Message function with the associated Exception_Occurrence
--      object.  Check that Function Exception_Information is available
--      to provide implementation-defined information about the exception
--      occurrence.
--
-- TEST DESCRIPTION:
--      This test checks that a message associated with a raised exception
--      is propagated with the exception, and can be retrieved using the
--      Exception_Message function.  The exception will be raised using the
--      'Identity attribute as a parameter to the Raise_Exception procedure,
--      and an associated message string will be provided.  The exception
--      will be handled, and the message associated with the occurrence will
--      be compared to the original source message (non-default).
--
--      The test also includes a simulated logging procedure
--      (Check_Exception_Information) that checks that Exception_Information
--      can be called.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      22 Jun 00   RLB     Added a check at Exception_Information can be
--                          called.
--
--!

with Report;
with Ada.Exceptions;

procedure CB41002 is
begin

   Report.Test ("CB41002", "Check that the message string input parameter " &
                           "in a call to the Raise_Exception procedure is " &
                           "associated with the raised exception "          &
                           "occurrence, and that the message string can "   &
                           "be obtained using the Exception_Message "       &
                           "function with the associated "                  &
                           "Exception_Occurrence object. Also check that "  &
                           "the Exception_Information function can be called");

   Test_Block:
   declare

      Number_Of_Exceptions : constant := 3;

      User_Exception_1,
      User_Exception_2,
      User_Exception_3 : exception;

      type String_Ptr is access String;

      User_Messages : constant array (1..Number_Of_Exceptions)
        of String_Ptr :=
        (new String'("Msg"),
         new String'("This message will override the default "   &
                     "message provided by the implementation"),
         new String'("The message can be captured by procedure"  & -- 200 chars
                      " Exception_Message.  It is designed to b" &
                      "e exactly 200 characters in length, sinc" &
                      "e there is a permission  concerning the " &
                      "truncation of a message over 200 chars. "));

      procedure Check_Exception_Information (
                 Occur : in Ada.Exceptions.Exception_Occurrence) is
          -- Simulates an error logging routine.
         Info : constant String :=
              Ada.Exceptions.Exception_Information (Occur);
         function Is_Substring_of (Target, Search : in String) return Boolean is
            -- Returns True if Search is a substring of Target, and False
            -- otherwise.
         begin
            for I in Report.Ident_Int(Target'First) ..
                     Target'Last - Search'Length + 1 loop
               if Target(I .. I+Search'Length-1) = Search then
                  return True;
               end if;
            end loop;
            return False;
         end Is_Substring_of;
      begin
         -- We can't display Info, as it often contains line breaks
         -- (confusing Report), and might look much like the failure of a test
         -- with an unhandled exception (thus confusing grading tools).
         --
         -- We don't particular care if the implementation advice is followed,
         -- but we make these checks to insure that a compiler cannot optimize
         -- away Info or the rest of this routine.
         if not Is_Substring_of (Info,
                       Ada.Exceptions.Exception_Name (Occur)) then
             Report.Comment ("Exception_Information does not contain " &
                             "Exception_Name - see 11.4.1(19)");
         elsif not Is_Substring_of (Info,
                       Ada.Exceptions.Exception_Message (Occur)) then
             Report.Comment ("Exception_Information does not contain " &
                             "Exception_Message - see 11.4.1(19)");
         end if;
      end Check_Exception_Information;

   begin

      for i in 1..Number_Of_Exceptions loop
         begin

            -- Raise a user-defined exception with a specific message string.
            case i is
               when 1 =>
                  Ada.Exceptions.Raise_Exception(User_Exception_1'Identity,
                                                 User_Messages(i).all);
               when 2 =>
                  Ada.Exceptions.Raise_Exception(User_Exception_2'Identity,
                                                 User_Messages(i).all);
               when 3 =>
                  Ada.Exceptions.Raise_Exception(User_Exception_3'Identity,
                                                 User_Messages(i).all);
               when others =>
                  Report.Failed("Incorrect result from Case statement");
            end case;

            Report.Failed
              ("Exception not raised by procedure Exception_With_Message " &
               "for User_Exception #" & Integer'Image(i));

         exception
            when Excptn : others =>

               begin
                  -- The message that is associated with the raising of each
                  -- exception is captured here using the Exception_Message
                  -- function.

                  if User_Messages(i).all /=
                     Ada.Exceptions.Exception_Message(Excptn)
                  then
                     Report.Failed
                       ("Message captured from exception is not the "  &
                        "message provided when the exception was raised, " &
                        "User_Exception #" & Integer'Image(i));
                  end if;

                  Check_Exception_Information(Excptn);
               end;
         end;
      end loop;



      -- Verify that the exception specific message is carried across
      -- various boundaries:

      begin

         begin
            Ada.Exceptions.Raise_Exception(User_Exception_1'Identity,
                                           User_Messages(1).all);
            Report.Failed("User_Exception_1 not raised");
         end;
         Report.Failed("User_Exception_1 not propagated");
      exception
         when Excptn : User_Exception_1 =>

            if User_Messages(1).all /=
               Ada.Exceptions.Exception_Message(Excptn)
            then
               Report.Failed("User_Message_1 not found");
            end if;
            Check_Exception_Information(Excptn);

         when others => Report.Failed("Unexpected exception handled - 1");
      end;



      begin

         begin
            Ada.Exceptions.Raise_Exception(User_Exception_2'Identity,
                                           User_Messages(2).all);
            Report.Failed("User_Exception_2 not raised");
         exception
            when Exc : User_Exception_2 =>

               -- The exception is reraised here; message should propagate
               -- with exception occurrence.

               Ada.Exceptions.Reraise_Occurrence(Exc);
            when others => Report.Failed("User_Exception_2 not handled");
         end;
         Report.Failed("User_Exception_2 not propagated");
      exception
         when Excptn : User_Exception_2 =>

            if User_Messages(2).all /=
               Ada.Exceptions.Exception_Message(Excptn)
            then
               Report.Failed("User_Message_2 not found");
            end if;
            Check_Exception_Information(Excptn);

         when others => Report.Failed("Unexpected exception handled - 2");
      end;


      -- Check exception and message propagation across task boundaries.

      declare

         task Raise_An_Exception is  -- single task
            entry Raise_It;
         end Raise_An_Exception;

         task body Raise_An_Exception is
         begin
            accept Raise_It do
               Ada.Exceptions.Raise_Exception(User_Exception_3'Identity,
                                              User_Messages(3).all);
            end Raise_It;
            Report.Failed("User_Exception_3 not raised");
         exception
            when Excptn : User_Exception_3 =>
               if User_Messages(3).all /=
                  Ada.Exceptions.Exception_Message(Excptn)
               then
                  Report.Failed
                    ("User_Message_3 not returned inside task body");
               end if;
               Check_Exception_Information(Excptn);
            when others =>
               Report.Failed("Incorrect exception raised in task body");
         end Raise_An_Exception;

      begin
         Raise_An_Exception.Raise_It;  -- Exception will be propagated here.
         Report.Failed("User_Exception_3 not propagated to caller");
      exception
         when Excptn : User_Exception_3 =>
            if User_Messages(3).all /=
               Ada.Exceptions.Exception_Message(Excptn)
            then
               Report.Failed("User_Message_3 not returned to caller of task");
            end if;
            Check_Exception_Information(Excptn);
         when others =>
            Report.Failed("Incorrect exception raised by task");
      end;


   exception
      when others => Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end CB41002;
