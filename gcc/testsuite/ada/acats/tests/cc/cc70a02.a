-- CC70A02.A
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
--      Check that the visible part of a generic formal package includes the
--      first list of basic declarative items of the package specification.
--      Check for a generic subprogram which declares a formal package with
--      (<>) as its actual part.
--
-- TEST DESCRIPTION:
--      The "first list of basic declarative items" of a package specification
--      is the visible part of the package. Thus, the declarations in the
--      visible part of the actual instance corresponding to a formal
--      package are available in the generic which declares the formal package.
--       
--      Declare a generic package which simulates a complex integer abstraction
--      (foundation code).
--
--      Declare a second generic package which defines a "signature" for
--      mathematical groups. Declare a generic function within a package
--      which utilizes the second generic package as a generic formal package
--      (with a (<>) actual_part).
--
--      In the main program, instantiate the first generic package, then
--      instantiate the second generic package with objects, types, and
--      operations declared in the first instance.
--
--      Instantiate the generic function and pass the second instance
--      to it as a generic actual parameter. Check that the instance of the
--      generic function performs as expected.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

generic               -- Mathematical group signature.

   type Group_Type is private;

   Identity : in Group_Type;

   with function Operation (Left, Right : Group_Type) return Group_Type;
   with function Inverse   (Right : Group_Type)       return Group_Type;

package CC70A02_0 is end;

-- No body for CC70A02_0.


     --==================================================================--


with CC70A02_0;       -- Mathematical group signature.

package CC70A02_1 is  -- Mathematical group operations.

   --                                  --
   -- Generic formal package used here --
   --                                  --

   generic            -- Powers for mathematical groups.
      with package Group is new CC70A02_0 (<>);
   function Power (Left : Group.Group_Type; Right : Integer)
     return Group.Group_Type;


end CC70A02_1;


     --==================================================================--


package body CC70A02_1 is  -- Mathematical group operations.



   function Power (Left : Group.Group_Type; Right : Integer)
     return Group.Group_Type is
      Result : Group.Group_Type := Group.Identity;
   begin
      for I in 1 .. abs(Right) loop                 -- Repeat group operations
         Result := Group.Operation (Result, Left);  -- the specified number of
      end loop;                                     -- times.

      if Right < 0 then                             -- If specified power is
         return Group.Inverse (Result);             -- negative, return the
      else                                          -- inverse of the result.
         return Result;                             -- If it is zero, return
      end if;                                       -- the identity.
   end Power;


end CC70A02_1;


     --==================================================================--


with Report;

with FC70A00;    -- Complex integer abstraction.
with CC70A02_0;  -- Mathematical group signature.
with CC70A02_1;  -- Mathematical group operations.

procedure CC70A02 is

   -- Declare an instance of complex integers:

   type My_Integer is range -100 .. 100;
   package Complex_Integers is new FC70A00 (My_Integer);


   -- Define an addition group for complex integers:

   package Complex_Addition_Group is new CC70A02_0
     (Group_Type => Complex_Integers.Complex_Type,  -- For complex integers...
      Identity   => Complex_Integers.Zero,          -- Additive identity.
      Operation  => Complex_Integers."+",           -- Additive operation.
      Inverse    => Complex_Integers."-");          -- Additive inverse.

   function Complex_Multiplication is new           -- Multiplication of a
     CC70A02_1.Power(Complex_Addition_Group);       -- complex integer by a
                                                    -- constant.


   -- Define a multiplication group for complex integers:

   package Complex_Multiplication_Group is new CC70A02_0
     (Group_Type => Complex_Integers.Complex_Type,  -- For complex integers...
      Identity   => Complex_Integers.One,           -- Multiplicative identity.
      Operation  => Complex_Integers."*",           -- Multiplicative oper.
      Inverse    => Complex_Integers.Reciprocal);   -- Multiplicative inverse.

   function Complex_Exponentiation is new           -- Exponentiation of a
     CC70A02_1.Power(Complex_Multiplication_Group); -- complex integer by a
                                                    -- constant.

   use Complex_Integers;


begin  -- Main program.

   Report.Test ("CC70A02", "Check that the visible part of a generic " &
                "formal package includes the first list of basic " &
                "declarative items of the package specification. Check " &
                "for a generic subprogram where formal package has (<>) " &
                "actual part");

   declare
      Mult_Operand         : constant Complex_Type := Complex ( -4,  9);
      Exp_Operand          : constant Complex_Type := Complex (  0, -7);

      Expected_Mult_Result : constant Complex_Type := Complex ( 28, -63);
      Expected_Exp_Result  : constant Complex_Type := Complex (-49,   0);
   begin

      if Complex_Multiplication (Mult_Operand, -7) /= Expected_Mult_Result then
         Report.Failed ("Incorrect results from complex multiplication");
      end if;

      if Complex_Exponentiation (Exp_Operand, 2) /= Expected_Exp_Result then
         Report.Failed ("Incorrect results from complex exponentiation");
      end if;

   end;

   Report.Result;

end CC70A02;
