-- CB30001.A
--
--                             Grant of Unlimited Rights
--
--     The Ada Conformity Assessment Authority (ACAA) holds unlimited
--     rights in the software and documentation contained herein. Unlimited
--     rights are the same as those granted by the U.S. Government for older
--     parts of the Ada Conformity Assessment Test Suite, and are defined
--     in DFAR 252.227-7013(a)(19). By making this public release, the ACAA
--     intends to confer upon all recipients unlimited rights equal to those
--     held by the ACAA. These rights include rights to use, duplicate,
--     release or disclose the released technical data and computer software
--     in whole or in part, in any manner and for any purpose whatsoever, and
--     to have or permit others to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS. THE ACAA MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--
--                                     Notice
--
--     The ACAA has created and maintains the Ada Conformity Assessment Test
--     Suite for the purpose of conformity assessments conducted in accordance
--     with the International Standard ISO/IEC 18009 - Ada: Conformity
--     assessment of a language processor. This test suite should not be used
--     to make claims of conformance unless used in accordance with
--     ISO/IEC 18009 and any applicable ACAA procedures.
--*
--
-- OBJECTIVE:
--      Check that the optional message string in a raise statement is
--      associated with the raised exception occurrence, and that the message
--      string can be obtained using the Exception_Message function with the
--      associated Exception_Occurrence object.
--
-- TEST DESCRIPTION:
--      This test checks that a message associated with a raised exception
--      is propagated with the exception, and can be retrieved using the
--      Exception_Message function.  The exception will be raised using
--      a raise statement with an associated message string.  The exception
--      will be handled, and the message associated with the occurrence will
--      be compared to the original source message (non-default).
--
--
-- CHANGE HISTORY:
--      20 Nov 14   RLB     Created test from existing CB41002 test.
--
--!

with Report;
with Ada.Exceptions;

procedure CB30001 is
begin

   Report.Test ("CB30001", "Check that the optional message string in a "   &
                           "raise statement is "                            &
                           "associated with the raised exception "          &
                           "occurrence, and that the message string can "   &
                           "be obtained using the Exception_Message "       &
                           "function with the associated "                  &
                           "Exception_Occurrence object");

   Test_Block:
   declare

      Number_Of_Exceptions : constant := 3;

      User_Exception_1,
      User_Exception_2,
      User_Exception_3 : exception;

      type String_Ptr is access String;

      User_Messages : constant array (1..Number_Of_Exceptions+1)
        of String_Ptr :=
        (new String'("Msg"),
         new String'("This message will override the default "   &
                     "message provided by the implementation"),
         new String'("The message can be captured by procedure"  & -- 200 chars
                      " Exception_Message.  It is designed to b" &
                      "e exactly 200 characters in length, sinc" &
                      "e there is a permission  concerning the " &
                      "truncation of a message over 200 chars. "),
         new String'("A different message than the one usually " &
                     "associated with this exception."));

   begin

      for i in 1..Number_Of_Exceptions loop
         begin

            -- Raise a user-defined exception with a specific message string.
            case i is
               when 1 =>
                  raise User_Exception_1 with User_Messages(i).all;
               when 2 =>
                  raise User_Exception_2 with User_Messages(i).all;
               when 3 =>
                  raise User_Exception_3 with User_Messages(i).all;
               when others =>
                  Report.Failed("Incorrect result from Case statement");
            end case;

            Report.Failed
              ("Exception not raised by raise statement " &
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
               end;
         end;
      end loop;



      -- Verify that the exception specific message is carried across
      -- various boundaries:

      begin

         begin
            raise User_Exception_1 with User_Messages(1).all;
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

         when others => Report.Failed("Unexpected exception handled - 1");
      end;


      begin

         begin
            raise User_Exception_2 with User_Messages(4).all;
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

            if User_Messages(4).all /=
               Ada.Exceptions.Exception_Message(Excptn)
            then
               Report.Failed("User_Message_4 not found");
            end if;

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
               raise User_Exception_3 with User_Messages(2).all;
            end Raise_It;
            Report.Failed("User_Exception_3 not raised");
         exception
            when Excptn : User_Exception_3 =>
               if User_Messages(2).all /=
                  Ada.Exceptions.Exception_Message(Excptn)
               then
                  Report.Failed
                    ("User_Message_2 not returned inside task body");
               end if;
            when others =>
               Report.Failed("Incorrect exception raised in task body");
         end Raise_An_Exception;

      begin
         Raise_An_Exception.Raise_It;  -- Exception will be propagated here.
         Report.Failed("User_Exception_3 not propagated to caller");
      exception
         when Excptn : User_Exception_3 =>
            if User_Messages(2).all /=
               Ada.Exceptions.Exception_Message(Excptn)
            then
               Report.Failed("User_Message_3 not returned to caller of task");
            end if;
         when others =>
            Report.Failed("Incorrect exception raised by task");
      end;


   exception
      when others => Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end CB30001;
