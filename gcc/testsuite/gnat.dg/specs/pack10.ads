-- { dg-do compile }

with Pack10_Pkg; use Pack10_Pkg;

package Pack10 is

   type Boolean_Vector is array (Positive range <>) of Boolean;

   type Packed_Boolean_Vector is new Boolean_Vector;
   pragma Pack (Packed_Boolean_Vector);

   procedure My_Proc is new Proc (Packed_Boolean_Vector);

end Pack10;
