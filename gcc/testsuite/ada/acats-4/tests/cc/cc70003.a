-- CC70003.A
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
--      Check that the actual passed to a formal package may be a formal
--      access-to-subprogram type. Check that the visible part of the generic
--      formal package includes the first list of basic declarative items of
--      the package specification.
--
-- TEST DESCRIPTION:
--      Declare a list abstraction in a generic package which manages lists of
--      elements of any nonlimited type (foundation code). Declare a generic
--      package which supports the execution of lists of operations. Provide
--      the generic package with two formal parameters: (1) a formal access-
--      to-function type, and (2) a generic formal package with the list
--      abstraction package as template. Within a procedure declared in the
--      list-execution package, utilize information about the profile of
--      the functions in the list. Declare a package which declares functions
--      matching the profile of the formal access-to-subprogram type. In the
--      main program, create a list of pointers to the functions declared in
--      the package, instantiate the list abstraction and list-execution
--      packages, and use the list-execution procedure to call each of the
--      functions in the list in sequence.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

generic
   type Element_Type is private;
package CC70003_0 is  -- This package simulates a generic list abstraction.

   -- The definition of List_Type below is purely artificial; its validity
   -- in the context of the abstraction is irrelevant to the feature being
   -- tested.

   type Element_Ptr is access Element_Type;

   subtype List_Size is Natural range 1 .. 2;
   type List_Type is array (List_Size) of Element_Ptr;

   function  View_Element (I : List_Size; L : List_Type) return Element_Type;

   procedure Write_Element (I : in     List_Size;
                            L : in out List_Type;
                            E : in     Element_Type);

   -- ... Other list operations for Element_Type.

end CC70003_0;


     --==================================================================--


package body CC70003_0 is

   -- The implementations of the operations below are purely artificial; the
   -- validity of their implementations in the context of the abstraction is
   -- irrelevant to the feature being tested.

   function View_Element (I : List_Size; L : List_Type) return Element_Type is
   begin
      return L(I).all;
   end View_Element;


   procedure Write_Element (I : in     List_Size;
                            L : in out List_Type;
                            E : in     Element_Type) is
   begin
      L(I) := new Element_Type'(E);
   end Write_Element;

end CC70003_0;


     --==================================================================--


with CC70003_0;    -- Generic list abstraction.
generic
   type Elem_Type is access function (F : Float) return Float;
   with package List_Mgr is new CC70003_0 (Elem_Type);
package CC70003_1 is  -- This package simulates support for executing lists
                      -- of operations.

   procedure Execute_List (L : List_Mgr.List_Type; F : in out Float);

   -- ... Other operations.

end CC70003_1;


     --==================================================================--


package body CC70003_1 is

   procedure Execute_List (L : List_Mgr.List_Type; F : in out Float) is
   begin
      for I in L'Range loop
         F := List_Mgr.View_Element(I, L)(F);   -- Execute next operation in
      end loop;                                 -- list with current value of
   end Execute_List;                            -- F as operand.


end CC70003_1;


     --==================================================================--


package CC70003_2 is

   function Sine (F : Float) return Float;
   function Exp  (F : Float) return Float;

   -- ... Other math functions.

end CC70003_2;


     --==================================================================--


package body CC70003_2 is

   -- The implementations of the functions below are purely artificial; the
   -- validity of their implementations in the context of the abstraction is
   -- irrelevant to the feature being tested.

   function Sine (F : Float) return Float is
   begin
      return (-0.15);
   end Sine;

   function Exp (F : Float) return Float is
   begin
      if (F = 0.0) then
         return (-0.69);
      else
         return (2.0);  -- This branch should be taken.
      end if;
   end Exp;

end CC70003_2;


     --==================================================================--


with CC70003_0;    -- Generic list abstraction.
with CC70003_1;  -- Generic operation-list abstraction.
with CC70003_2;  -- Math library.

with Report;
procedure CC70003 is

   type Math_Op is access function (F : Float) return Float;

   package Math_Op_Lists        is new CC70003_0 (Math_Op);
   package Math_Op_List_Support is new CC70003_1 (Math_Op, Math_Op_Lists);

   Sin_Ptr : Math_Op := CC70003_2.Sine'Access;
   Exp_Ptr : Math_Op := CC70003_2.Exp'Access;

   Op_List : Math_Op_Lists.List_Type;

   Operand  : Float := 0.0;
   Expected : Float := 2.0;


begin
   Report.Test ("CC70003", "Check that the actual passed to a formal " &
                "package may be a formal access-to-subprogram type");

   Math_Op_Lists.Write_Element (1, Op_List, Sin_Ptr);
   Math_Op_Lists.Write_Element (2, Op_List, Exp_Ptr);

   Math_Op_List_Support.Execute_List (Op_List, Operand);

   if (Operand /= Expected) then
      Report.Failed ("Incorrect results from indirect function calls");
   end if;

   Report.Result;
end CC70003;
