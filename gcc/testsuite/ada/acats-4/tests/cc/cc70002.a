-- CC70002.A
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
--      Check that a formal package actual part may specify actual parameters
--      for a generic formal package. Check that these actual parameters may
--      be formal types, formal objects, and formal subprograms. Check that
--      the visible part of the generic formal package includes the first list
--      of basic declarative items of the package specification, and that if
--      the formal package actual part is (<>), it also includes the generic
--      formal part of the template for the formal package.
--
-- TEST DESCRIPTION:
--      Declare a generic package which defines a "signature" for mathematical
--      groups. Declare a second generic package which defines a
--      two-dimensional matrix abstraction. Declare a third generic package
--      which provides mathematical group operations for two-dimensional
--      matrices. Provide this third generic with two formal parameters: (1)
--      a generic formal package with the second generic as template and a
--      (<>) actual part, and (2) a generic formal package with the first
--      generic as template and an actual part that takes a formal type,
--      object, and subprogram from the first formal package as actuals.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

generic              -- Mathematical group signature.

   type Group_Type is private;

   Identity : in Group_Type;

   with function Operation (Left, Right : Group_Type) return Group_Type;
-- with function Inverse... (omitted for brevity).

package CC70002_0 is

   function Power (Left : Group_Type; Right : Integer) return Group_Type;

   -- ... Other group operations.

end CC70002_0;


     --==================================================================--


package body CC70002_0 is

   -- The implementation of Power is purely artificial; the validity of its
   -- implementation in the context of the abstraction is irrelevant to the
   -- feature being tested.

   function Power (Left : Group_Type; Right : Integer) return Group_Type is
      Result : Group_Type := Identity;
   begin
      Result := Operation (Result, Left);  -- All this really does is add
      return Result;                       -- one to each matrix element.
   end Power;

end CC70002_0;


     --==================================================================--


generic          -- 2D matrix abstraction.
   type Element_Type is range <>;

   type Abscissa is range <>;
   type Ordinate is range <>;

   type Matrix_2D is array (Abscissa, Ordinate) of Element_Type;
package CC70002_1 is

   Add_Ident : constant Matrix_2D := (Abscissa => (others => 1));
                                                          -- Artificial for
                                                          -- testing purposes.
   -- ... Other identity matrices.


   function "+" (A, B : Matrix_2D) return Matrix_2D;

   -- ... Other operations.

end CC70002_1;


     --==================================================================--


package body CC70002_1 is

   function "+" (A, B : Matrix_2D) return Matrix_2D is
      C : Matrix_2D;
   begin
      for I in Abscissa loop
         for J in Ordinate loop
            C(I,J) := A(I,J) + B(I,J);
         end loop;
      end loop;
      return C;
   end "+";

end CC70002_1;


     --==================================================================--


with CC70002_0;  -- Mathematical group signature.
with CC70002_1;  -- 2D matrix abstraction.

generic          -- Mathematical 2D matrix addition group.

   with package Matrix_Ops is new CC70002_1 (<>);

   -- Although the restriction of the formal package below to signatures
   -- describing addition groups, and then only for 2D matrices, is rather
   -- artificial in the context of this "application," the passing of types,
   -- objects, and subprograms as actuals to a formal package is not.

   with package Math_Sig is new CC70002_0
     (Group_Type => Matrix_Ops.Matrix_2D,
      Identity   => Matrix_Ops.Add_Ident,
      Operation  => Matrix_Ops."+");

package CC70002_2 is

   -- Add two matrices that are to be multiplied by coefficients:
   -- [ ] = CA*[ ] + CB*[ ].

   function Add_Matrices_With_Coefficients (A  : Matrix_Ops.Matrix_2D;
                                            CA : Integer;
                                            B  : Matrix_Ops.Matrix_2D;
                                            CB : Integer)
     return Matrix_Ops.Matrix_2D;

   -- ...Other operations.

end CC70002_2;


     --==================================================================--


package body CC70002_2 is

   function Add_Matrices_With_Coefficients (A  : Matrix_Ops.Matrix_2D;
                                            CA : Integer;
                                            B  : Matrix_Ops.Matrix_2D;
                                            CB : Integer)
     return Matrix_Ops.Matrix_2D is
      Left, Right : Matrix_Ops.Matrix_2D;
   begin
      Left  := Math_Sig.Power (A, CA);      -- Multiply 1st array by its coeff.
      Right := Math_Sig.Power (B, CB);      -- Multiply 2nd array by its coeff.
      return (Matrix_Ops."+" (Left, Right));-- Add these two arrays.
   end Add_Matrices_With_Coefficients;

end CC70002_2;


     --==================================================================--


with CC70002_0;  -- Mathematical group signature.
with CC70002_1;  -- 2D matrix abstraction.
with CC70002_2;  -- Mathematical 2D matrix addition group.

with Report;
procedure CC70002 is

   subtype Cell_Type     is Positive range 1 .. 3;
   subtype Category_Type is Positive range 1 .. 2;

   type Data_Points is new Natural range 0 .. 100;

   type Table_Type is array (Cell_Type, Category_Type) of Data_Points;

   package Data_Table_Support is new CC70002_1 (Data_Points,
                                                Cell_Type,
                                                Category_Type,
                                                Table_Type);

   package Data_Table_Addition_Group is new CC70002_0
     (Group_Type => Table_Type,
      Identity   => Data_Table_Support.Add_Ident,
      Operation  => Data_Table_Support."+");

   package Table_Add_Ops is new CC70002_2
     (Data_Table_Support, Data_Table_Addition_Group);


   Scores_Table : Table_Type := ( (  12,   0),
                                  (  21,  33),
                                  (  49,   9) );
   Expected     : Table_Type := ( (  26,   2),
                                  (  44,  68),
                                  ( 100,  20) );

begin
   Report.Test ("CC70002", "Check that a generic formal package actual " &
                "part may specify formal objects, formal subprograms, " &
                "and formal types");

   Scores_Table := Table_Add_Ops.Add_Matrices_With_Coefficients
                     (Scores_Table, 2,
                      Scores_Table, 1);

   if (Scores_Table /= Expected) then
      Report.Failed ("Incorrect result for multi-dimensional array");
   end if;

   Report.Result;
end CC70002;
