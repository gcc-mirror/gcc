-- CA11D013.AM
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
--      Check that a child unit can raise an exception that is declared in 
--      parent.                
--
-- TEST DESCRIPTION:
--      Declare a package which defines complex number abstraction with
--      user-defined exceptions (foundation code).
--
--      Add a public child package to the above package. Declare two 
--      subprograms for the parent type.  Each of the subprograms raises a 
--      different exception, based on the value of an input parameter.
--
--      Add a public child procedure to the foundation package.  This
--      procedure raises an exception based on the value of an input 
--      parameter.
--
--      Add a public child function to the foundation package.  This
--      function raises an exception based on the value of an input 
--      parameter.
--
--      In the main program, "with" the child packages, then check that
--      the exceptions are raised and handled as expected.  Ensure that
--      exceptions are:
--         1) raised in the public child package and handled/reraised to
--            be handled by the main program.
--         2) raised and handled locally in the public child package.
--         3) raised and handled locally by "others" in the public child 
--            procedure.
--         4) raised in the public child function and propagated to the
--            main program.
--
-- TEST FILES:
--      The following files comprise this test:
--
--         FA11D00.A
--         CA11D010.A
--         CA11D011.A
--         CA11D012.A
--      => CA11D013.AM
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

with FA11D00.CA11D010;               -- Add_Subtract_Complex
with FA11D00.CA11D011;               -- Multiply_Complex
with FA11D00.CA11D012;               -- Divide_Complex

with Report;


procedure CA11D013 is

   package Complex_Pkg renames FA11D00;
   package Add_Subtract_Complex_Pkg renames FA11D00.CA11D010;
   use Complex_Pkg;

begin

   Report.Test ("CA11D013", "Check that a child unit can raise an " &
                "exception that is declared in parent");


   Add_Complex_Subtest:
   declare
      First       : Complex_Type := Complex (Int_Type (Report.Ident_Int (3)), 
                                    Int_Type (Report.Ident_Int (7)));  
      Second      : Complex_Type := Complex (Int_Type (Report.Ident_Int (5)), 
                                    Int_Type (Report.Ident_Int (3)));  
      Add_Result  : Complex_Type := Complex (Int_Type (Report.Ident_Int (8)), 
                                    Int_Type (Report.Ident_Int (10)));  
      Third       : Complex_Type := Complex (Int_Type(Report.Ident_Int(-100)), 
                                    Int_Type (Report.Ident_Int (100)));  
      Complex_Num : Complex_Type := Zero;  

   begin
      Add_Subtract_Complex_Pkg.Add (First, Second, Complex_Num);

      if (Complex_Num /= Add_Result) then
         Report.Failed ("Incorrect results from addition");
      end if;
  
      -- Error is raised in child package and exception 
      -- will be handled/reraised to caller.

      Add_Subtract_Complex_Pkg.Add (First, Third, Complex_Num);

      -- Error was not raised in child package.
      Report.Failed ("Exception was not reraised in addition");

   exception
      when Add_Error     => 
         if not TC_Handled_In_Child_Pkg_Proc then
            Report.Failed ("Exception was not raised in addition");
         else
            TC_Handled_In_Caller := true;  -- Exception is reraised from
                                           -- child package.
         end if;

      when others => 
         Report.Failed ("Unexpected exception in addition subtest"); 
         TC_Handled_In_Caller := false;  -- Improper exception handling
                                         -- in caller.

   end Add_Complex_Subtest;


   Subtract_Complex_Subtest:
   declare
      First       : Complex_Type := Complex (Int_Type (Report.Ident_Int (3)), 
                                    Int_Type (Report.Ident_Int (6)));  
      Second      : Complex_Type := Complex (Int_Type (Report.Ident_Int (5)), 
                                    Int_Type (Report.Ident_Int (7)));  
      Sub_Result  : Complex_Type := Complex (Int_Type (Report.Ident_Int (2)), 
                                    Int_Type (Report.Ident_Int (1)));  
      Third       : Complex_Type := Complex (Int_Type(Report.Ident_Int(-200)), 
                                    Int_Type (Report.Ident_Int (1)));  
      Complex_Num : Complex_Type;

   begin
      Complex_Num := Add_Subtract_Complex_Pkg.Subtract (Second, First);

      if (Complex_Num /= Sub_Result) then
         Report.Failed ("Incorrect results from subtraction");
      end if;
  
      -- Error is raised and exception will be handled in child package.
      Complex_Num := Add_Subtract_Complex_Pkg.Subtract (Second, Third);

   exception
      when Subtract_Error => 
         Report.Failed ("Exception raised in subtraction and " &
                        "propagated to caller");
         TC_Handled_In_Child_Pkg_Func := false; -- Improper exception handling
                                                -- in caller.

      when others => 
         Report.Failed ("Unexpected exception in subtraction subtest"); 
         TC_Handled_In_Child_Pkg_Func := false; -- Improper exception handling
                                                -- in caller.

   end Subtract_Complex_Subtest;


   Multiply_Complex_Subtest:
   declare
      First       : Complex_Type := Complex (Int_Type(Report.Ident_Int(3)), 
                                    Int_Type (Report.Ident_Int (4)));  
      Second      : Complex_Type := Complex (Int_Type(Report.Ident_Int(5)), 
                                    Int_Type (Report.Ident_Int (3)));  
      Mult_Result : Complex_Type := Complex(Int_Type(Report.Ident_Int(15)), 
                                    Int_Type(Report.Ident_Int (12)));  
      Third       : Complex_Type := Complex(Int_Type(Report.Ident_Int(10)), 
                                    Int_Type(Report.Ident_Int (-10)));  
      Complex_Num : Complex_Type;

   begin
      CA11D011 (First, Second, Complex_Num);

      if (Complex_Num /= Mult_Result) then
         Report.Failed ("Incorrect results from multiplication");
      end if;
  
      -- Error is raised and exception will be handled in child package.
     CA11D011 (First, Third, Complex_Num);

   exception
      when Multiply_Error => 
         Report.Failed ("Exception raised in multiplication and " &
                        "propagated to caller");
         TC_Handled_In_Child_Sub := false;     -- Improper exception handling
                                               -- in caller.

      when others => 
         Report.Failed ("Unexpected exception in multiplication subtest"); 
         TC_Handled_In_Child_Sub := false;     -- Improper exception handling
                                               -- in caller.
   end Multiply_Complex_Subtest;


   Divide_Complex_Subtest:
   declare
      First       : Complex_Type := Complex (Int_Type (Report.Ident_Int(10)), 
                                    Int_Type (Report.Ident_Int (15)));  
      Second      : Complex_Type := Complex (Int_Type(Report.Ident_Int(5)), 
                                    Int_Type (Report.Ident_Int (3)));  
      Div_Result  : Complex_Type := Complex (Int_Type(Report.Ident_Int(2)), 
                                    Int_Type (Report.Ident_Int (5)));  
      Third       : Complex_Type := Complex (Int_Type(Report.Ident_Int(-10)), 
                                    Int_Type (Report.Ident_Int (0)));  
      Complex_Num : Complex_Type := Zero;

   begin
      Complex_Num := CA11D012 (First, Second);

      if (Complex_Num /= Div_Result) then
         Report.Failed ("Incorrect results from division");
      end if;
  
      -- Error is raised in child package; exception will be
      -- propagated to caller.
      Complex_Num := CA11D012 (Second, Third);

      -- Error was not raised in child package.
      Report.Failed ("Exception was not raised in division subtest ");

   exception
      when Divide_Error => 
         TC_Propagated_To_Caller := true;  -- Exception is propagated.

      when others => 
         Report.Failed ("Unexpected exception in division subtest"); 
         TC_Propagated_To_Caller := false;  -- Improper exception handling
                                            -- in caller.
   end Divide_Complex_Subtest;


   if not (TC_Handled_In_Caller         and     -- Check to see that all 
           TC_Handled_In_Child_Pkg_Proc and     -- exceptions were handled in
           TC_Handled_In_Child_Pkg_Func and     -- the proper locations.
           TC_Handled_In_Child_Sub      and     
           TC_Propagated_To_Caller)
   then
      Report.Failed ("Exceptions handled in incorrect locations");
   end if;

   Report.Result;

end CA11D013;
