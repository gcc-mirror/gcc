-- CB41001.A
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
--      Check that the 'Identity attribute returns the unique identity of an
--      exception. Check that the Raise_Exception procedure can raise an 
--      exception that is specified through the use of the 'Identity attribute,
--      and that Reraise_Occurrence can re-raise an exception occurrence
--      using an exception choice parameter.
--
-- TEST DESCRIPTION:
--      This test uses the capability of the 'Identity attribute, which 
--      returns the unique identity of an exception, as an Exception_Id 
--      result.  This result is used as an input parameter to the procedure
--      Raise_Exception.  The exception that results is handled, propagated
--      using the Reraise_Occurrence procedure, and handled again.
--      The above actions are performed for both a user-defined and a
--      predefined exception.
--
--       
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      11 Nov 96   SAIC    ACVC 2.1: Modified Propagate_User_Exception.
--
--!

with Report;
with Ada.Exceptions;

procedure CB41001 is

begin

   Report.Test ("CB41001", "Check that the 'Identity attribute returns " &
                           "the unique identity of an exception. Check " &
                           "that the 'Identity attribute is of type "    &
                           "Exception_Id.  Check that the "              &
                           "Raise_Exception procedure can raise an "     &
                           "exception that is specified through the "    &
                           "use of the 'Identity attribute");
   Test_Block:
   declare

      Check_Points : constant := 5;

      type Check_Point_Array_Type is array (1..Check_Points) of Boolean;

      -- Global array used to track the processing path through the test.
      TC_Check_Points : Check_Point_Array_Type := (others => False);

      A_User_Defined_Exception  : Exception;
      An_Exception_ID           : Ada.Exceptions.Exception_Id :=
                                    Ada.Exceptions.Null_Id;

      procedure Propagate_User_Exception is
         Hidden_Exception : Exception;
      begin
         -- Use the 'Identity function to store the unique identity of a
         -- user defined exception into a variable of type Exception_Id.

         An_Exception_ID := A_User_Defined_Exception'Identity;

         -- Raise this user defined exception using the result of the 
         -- 'Identity attribute.

         Ada.Exceptions.Raise_Exception(E => An_Exception_Id);

         Report.Failed("User defined exception not raised by " &
                       "procedure Propagate_User_Exception");

      exception
         when Proc_Excpt : A_User_Defined_Exception => -- Expected exception.
            begin

               -- By raising a different exception at this point, the 
               -- information associated with A_User_Defined_Exception must
               -- be correctly stacked internally.

               Ada.Exceptions.Raise_Exception(Hidden_Exception'Identity);
               Report.Failed("Hidden_Exception not raised by " &
                             "procedure Propagate_User_Exception");
            exception
               when others => 
                  TC_Check_Points(1) := True;

                  -- Reraise the original exception, which will be propagated 
                  -- outside the scope of this procedure.

                  Ada.Exceptions.Reraise_Occurrence(Proc_Excpt);
                  Report.Failed("User defined exception not reraised");

            end;

         when others =>
            Report.Failed("Unexpected exception raised by " &
                          "Procedure Propagate_User_Exception");
      end Propagate_User_Exception;

   begin

      User_Exception_Block:
      begin
         -- Call procedure to raise, handle, and reraise a user defined 
         -- exception.
         Propagate_User_Exception;

         Report.Failed("User defined exception not propagated from " &
                       "procedure Propagate_User_Exception");

      exception
         when A_User_Defined_Exception => -- Expected exception.
            TC_Check_Points(2) := True;
         when others =>
            Report.Failed
              ("Unexpected exception handled in User_Exception_Block");
      end User_Exception_Block;


      Predefined_Exception_Block:
      begin

         Inner_Block:
         begin

            begin
               -- Use the 'Identity attribute as an input parameter to the
               -- Raise_Exception procedure.

               Ada.Exceptions.Raise_Exception(Constraint_Error'Identity);
               Report.Failed("Constraint_Error not raised in Inner_Block");

            exception
               when Excpt : Constraint_Error =>  -- Expected exception.
                  TC_Check_Points(3) := True;

                  -- Reraise the exception.
                  Ada.Exceptions.Reraise_Occurrence(X => Excpt);
                  Report.Failed("Predefined exception not raised from " &
                                "within the exception handler - 1");
               when others => 
                  Report.Failed("Incorrect result from attempt to raise " &
                                "Constraint_Error using the 'Identity "   &
                                "attribute - 1");
            end;

            Report.Failed("Constraint_Error not reraised in Inner_Block");

         exception
            when Block_Excpt : Constraint_Error =>  -- Expected exception.
               TC_Check_Points(4) := True;

               -- Reraise the exception in a scope where the exception
               -- was not originally raised.

               Ada.Exceptions.Reraise_Occurrence(X => Block_Excpt);
               Report.Failed("Predefined exception not raised from " &
                             "within the exception handler - 2");

            when others => 
               Report.Failed("Incorrect result from attempt to raise " &
                             "Constraint_Error using the 'Identity "   &
                             "attribute - 2");
         end Inner_Block;

         Report.Failed("Exception not propagated from Inner_Block");

      exception
         when Constraint_Error =>  -- Expected exception.
            TC_Check_Points(5) := True;
         when others => 
            Report.Failed("Unexpected exception handled after second " &
                          "reraise of Constraint_Error");
      end Predefined_Exception_Block;


      -- Verify the processing path taken through the test.

      for i in 1..Check_Points loop
         if not TC_Check_Points(i) then
            Report.Failed("Incorrect processing path taken through test, " &
                          "didn't pass check point #" & Integer'Image(i));
         end if;
      end loop;

   exception
      when others => Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end CB41001;
