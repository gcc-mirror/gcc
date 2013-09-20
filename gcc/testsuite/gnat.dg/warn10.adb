-- { dg-do compile }
-- { dg-options "-O3 -gnatn -Winline" }

package body Warn10 is

   procedure Do_Something(Driver : My_Driver) is
      X : Float;
   begin
       X := Get_Input_Value( Driver, 1, 1);
   end;

end Warn10;
