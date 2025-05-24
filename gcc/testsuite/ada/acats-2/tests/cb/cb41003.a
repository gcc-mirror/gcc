-- CB41003.A
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
--      Check that an exception occurrence can be saved into an object of
--      type Exception_Occurrence using the procedure Save_Occurrence.
--      Check that a saved exception occurrence can be used to reraise 
--      another occurrence of the same exception using the procedure
--      Reraise_Occurrence.  Check that the function Save_Occurrence will
--      allocate a new object of type Exception_Occurrence_Access, and saves
--      the source exception to the new object which is returned as the 
--      function result.
--
-- TEST DESCRIPTION:
--      This test verifies that an occurrence of an exception can be saved,
--      using either of two overloaded versions of Save_Occurrence.  The
--      procedure version of Save_Occurrence is used to save an occurrence
--      of a user defined exception into an object of type 
--      Exception_Occurrence.  This object is then used as an input 
--      parameter to procedure Reraise_Occurrence, the expected exception is
--      handled, and the exception id of the handled exception is compared
--      to the id of the originally raised exception.
--      The function version of Save_Occurrence returns a result of 
--      Exception_Occurrence_Access, and is used to store the value of another
--      occurrence of the user defined exception.  The resulting access value
--      is dereferenced and used as an input to Reraise_Occurrence.  The
--      resulting exception is handled, and the exception id of the handled 
--      exception is compared to the id of the originally raised exception.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

with Report;
with Ada.Exceptions;

procedure CB41003 is

begin

   Report.Test ("CB41003", "Check that an exception occurrence can "   &
                           "be saved into an object of type "          &
                           "Exception_Occurrence using the procedure " &
                           "Save_Occurrence");

   Test_Block:
   declare

      use Ada.Exceptions;

      User_Exception_1,
      User_Exception_2   : Exception;

      Saved_Occurrence   : Exception_Occurrence;
      Occurrence_Ptr     : Exception_Occurrence_Access;

      User_Message       : constant String :=   -- 200 character string.
        "The string returned by Exception_Message may be tr" &
        "uncated (to no less then 200 characters) by the Sa" &
        "ve_Occurrence procedure (not the function), the Re" &
        "raise_Occurrence proc, and the re-raise statement.";

   begin

      Raise_And_Save_Block_1 :
      begin

         -- This nested exception structure is designed to ensure that the
         -- appropriate exception occurrence is saved using the 
         -- Save_Occurrence procedure.

         raise Program_Error;
         Report.Failed("Program_Error not raised");

      exception
         when Program_Error => 

            begin
               -- Use the procedure Raise_Exception, along with the 'Identity
               -- attribute to raise the first user defined exception.  Note
               -- that a 200 character message is included in the call.

               Raise_Exception(User_Exception_1'Identity, User_Message);
               Report.Failed("User_Exception_1 not raised");

            exception
               when Exc : User_Exception_1 => 

                  -- This exception occurrence is saved into a variable using
                  -- procedure Save_Occurrence.  This saved occurrence should
                  -- not be confused with the raised occurrence of 
                  -- Program_Error above.

                  Save_Occurrence(Target => Saved_Occurrence, Source => Exc);

               when others => 
                  Report.Failed("Unexpected exception handled, expecting " &
                                "User_Exception_1");
            end;

         when others => 
            Report.Failed("Incorrect exception generated by raise statement");

      end Raise_And_Save_Block_1;


      Reraise_And_Handle_Saved_Exception_1 :
      begin
         -- Reraise the exception that was saved in the previous block.

         Reraise_Occurrence(X => Saved_Occurrence);

      exception
         when Exc : User_Exception_1 => -- Expected exception.
            -- Check the exception id of the handled id by using the 
            -- Exception_Identity function, and compare with the id of the
            -- originally raised exception.

            if User_Exception_1'Identity /= Exception_Identity(Exc) then
               Report.Failed("Exception_Ids do not match - 1");
            end if;

            -- Check that the message associated with this exception occurrence
            -- has not been truncated (it was originally 200 characters).

            if User_Message /= Exception_Message(Exc) then
               Report.Failed("Exception messages do not match - 1");
            end if;

         when others => 
            Report.Failed
              ("Incorrect exception raised by Reraise_Occurrence - 1");
      end Reraise_And_Handle_Saved_Exception_1;


      Raise_And_Save_Block_2 :
      begin

         Raise_Exception(User_Exception_2'Identity, User_Message);
         Report.Failed("User_Exception_2 not raised");

      exception
         when Exc : User_Exception_2 => 

            -- This exception occurrence is saved into an access object 
            -- using function Save_Occurrence.

            Occurrence_Ptr := Save_Occurrence(Source => Exc);

         when others => 
            Report.Failed("Unexpected exception handled, expecting " &
                          "User_Exception_2");
      end Raise_And_Save_Block_2;


      Reraise_And_Handle_Saved_Exception_2 :
      begin
         -- Reraise the exception that was saved in the previous block.
         -- Dereference the access object for use as input parameter.

         Reraise_Occurrence(X => Occurrence_Ptr.all);

      exception
         when Exc : User_Exception_2 => -- Expected exception.
            -- Check the exception id of the handled id by using the 
            -- Exception_Identity function, and compare with the id of the
            -- originally raised exception.

            if User_Exception_2'Identity /= Exception_Identity(Exc) then
               Report.Failed("Exception_Ids do not match - 2");
            end if;

            -- Check that the message associated with this exception occurrence
            -- has not been truncated (it was originally 200 characters).

            if User_Message /= Exception_Message(Exc) then
               Report.Failed("Exception messages do not match - 2");
            end if;

         when others => 
            Report.Failed
              ("Incorrect exception raised by Reraise_Occurrence - 2");
       end Reraise_And_Handle_Saved_Exception_2;


       -- Another example of the use of saving an exception occurrence
       -- is demonstrated in the following block, where the ability to 
       -- save an occurrence into a data structure, for later processing,
       -- is modeled.

       Store_And_Handle_Block:
       declare
          
          Exc_Number  : constant := 3;
          Exception_1, 
          Exception_2, 
          Exception_3 : exception;

          Exception_Storage : array (1..Exc_Number) of Exception_Occurrence;
          Messages          : array (1..Exc_Number) of String(1..9) :=
                                ("Message 1", "Message 2", "Message 3");

       begin

          Outer_Block:
          begin

             Inner_Block:
             begin

                for i in 1..Exc_Number loop
                   begin

                      begin
                         -- Exceptions all raised in a deep scope.
                         if i = 1 then
                            Raise_Exception(Exception_1'Identity, Messages(i));
                         elsif i = 2 then
                            Raise_Exception(Exception_2'Identity, Messages(i));
                         elsif i = 3 then
                            Raise_Exception(Exception_3'Identity, Messages(i));
                         end if;
                         Report.Failed("Exception not raised on loop #" &
                                       Integer'Image(i));
                      end;
                      Report.Failed("Exception not propagated on loop #" &
                                    Integer'Image(i));
                   exception
                      when Exc : others =>

                         -- Save each occurrence into a storage array for 
                         -- later processing.

                         Save_Occurrence(Exception_Storage(i), Exc);
                   end;
                end loop;

             end Inner_Block;
          end Outer_Block;

          -- Raise the exceptions from the stored occurrences, and handle.

          for i in 1..Exc_Number loop
             begin
                Reraise_Occurrence(Exception_Storage(i));
                Report.Failed("No exception reraised for " &
                              "exception #" & Integer'Image(i));
             exception
                when Exc   : others =>
                   -- The following sequence of checks ensures that the 
                   -- correct occurrence was stored, and the associated
                   -- exception was raised and handled in the proper order.
                   if i = 1 then
                      if Exception_1'Identity /= Exception_Identity(Exc) then
                         Report.Failed("Exception_1 not raised");
                      end if;
                   elsif i = 2 then
                      if Exception_2'Identity /= Exception_Identity(Exc) then
                         Report.Failed("Exception_2 not raised");
                      end if;
                   elsif i = 3 then
                      if Exception_3'Identity /= Exception_Identity(Exc) then
                         Report.Failed("Exception_3 not raised");
                      end if;
                   end if;

                   if Exception_Message(Exc) /= Messages(i) then
                      Report.Failed("Incorrect message associated with " &
                                    "exception #" & Integer'Image(i));
                   end if;
             end;
          end loop;
       exception
          when others => 
            Report.Failed("Unexpected exception in Store_And_Handle_Block");
       end Store_And_Handle_Block;


      Reraise_Out_Of_Scope:
      declare

         TC_Value      : constant := 5;
         The_Exception : exception;
         Saved_Exc_Occ : Exception_Occurrence;

         procedure Handle_It (Exc_Occ : in Exception_Occurrence) is
            Must_Be_Raised : exception;
         begin
            if Exception_Identity(Exc_Occ) = The_Exception'Identity then
               raise Must_Be_Raised;
               Report.Failed("Exception Must_Be_Raised was not raised");
            else
               Report.Failed("Incorrect exception handled in " &
                             "Procedure Handle_It");
            end if;
         end Handle_It;

      begin

         if Report.Ident_Int(5) = TC_Value then
            raise The_Exception;
         end if;

      exception
         when Exc : others => 
            Save_Occurrence (Saved_Exc_Occ, Exc);
            begin
               Handle_It(Saved_Exc_Occ);   -- Raise another exception, in a
            exception                      -- different scope.
               when others =>              -- Handle this new exception.
                  begin
                     Reraise_Occurrence (Saved_Exc_Occ);  -- Reraise the
                                                          -- original excptn.
                     Report.Failed("Saved Exception was not raised");
                  exception                               
                     when Exc_2 : others =>
                        if Exception_Identity (Exc_2) /= 
                           The_Exception'Identity 
                        then
                           Report.Failed
                             ("Incorrect exception occurrence reraised");
                        end if;
                  end;
            end;
      end Reraise_Out_Of_Scope;


   exception
      when others => Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end CB41003;
