-- C3A0006.A
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
--      Check that access to subprogram may be stored within data 
--      structures, and that the access to subprogram can subsequently 
--      be called. 
-- 
-- TEST DESCRIPTION:
--      Declare an access to function type in a package specification.  
--      Declare an array of the access type.  Declare three different 
--      functions that can be referred to by the access to function type.  
--      
--      In the main program, declare a key function that builds the array 
--      by calling each function indirectly through the access value.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!


package C3A0006_0 is

   TC_Sine_Call  : Integer := 0;
   TC_Cos_Call   : Integer := 0;
   TC_Tan_Call   : Integer := 0;

   Sine_Value    : Float :=  4.0;
   Cos_Value     : Float :=  8.0;
   Tan_Value     : Float := 10.0;

   -- Type accesses to any function
   type Trig_Function_Ptr is access function        
      (Angle : in Float) return Float;

   function Sine (Angle : in Float) return Float;         

   function Cos  (Angle : in Float) return Float;        

   function Tan  (Angle : in Float) return Float;         

end C3A0006_0;


-----------------------------------------------------------------------------


package body C3A0006_0 is

   function Sine (Angle : in Float) return Float is
   begin
     TC_Sine_Call := TC_Sine_Call + 1;
     Sine_Value := Sine_Value + Angle;
     return Sine_Value;
   end Sine;


   function Cos  (Angle: in Float) return Float is
   begin
     TC_Cos_Call := TC_Cos_Call + 1;
     Cos_Value := Cos_Value - Angle;
     return Cos_Value;
   end Cos;


   function Tan (Angle : in Float) return Float is
   begin
     TC_Tan_Call := TC_Tan_Call + 1;
     Tan_Value := (Tan_Value + (Tan_Value * Angle));
     return Tan_Value;
   end Tan;


end C3A0006_0;

-----------------------------------------------------------------------------


with Report;

with C3A0006_0; 

procedure C3A0006 is

   Trig_Value, Theta  : Float := 0.0;

   Total_Routines     : constant := 3;

   Sine_Total         : constant := 7.0;
   Cos_Total          : constant := 5.0;
   Tan_Total          : constant := 75.0;

   Trig_Table : array (1 .. Total_Routines) of C3A0006_0.Trig_Function_Ptr;


   -- Key function to build the table
   function Call_Trig_Func (Func : C3A0006_0.Trig_Function_Ptr;
                            Operand : Float) return Float is 
   begin
      return (Func(Operand));
   end Call_Trig_Func;


begin

   Report.Test ("C3A0006", "Check that access to subprogram may be " &
                "stored within data structures, and that the access " &
                "to subprogram can subsequently be called");

   Trig_Table := (C3A0006_0.Sine'Access, C3A0006_0.Cos'Access, 
                  C3A0006_0.Tan'Access);

   -- increase the value of Theta to build the table
   for I in 1 .. Total_Routines loop
      Theta := Theta + 0.5;
      for J in 1 .. Total_Routines loop
         Trig_Value     := Call_Trig_Func (Trig_Table(J), Theta);
      end loop;
   end loop;

   if C3A0006_0.TC_Sine_Call /= Total_Routines 
     or C3A0006_0.TC_Cos_Call /= Total_Routines
     or C3A0006_0.TC_Tan_Call /= Total_Routines then
        Report.Failed ("Incorrect subprograms result");
   end if;

   if C3A0006_0.Sine_Value /= Sine_Total
     or C3A0006_0.Cos_Value /= Cos_Total
     or C3A0006_0.Tan_Value /= Tan_Total then
        Report.Failed ("Incorrect values returned from subprograms");
   end if;

   if Trig_Value /= Tan_Total then
        Report.Failed ("Incorrect call order.");
   end if;

   Report.Result;

end C3A0006;
