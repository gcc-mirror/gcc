-- { dg-do compile }
-- { dg-options "-gnatct" }

with Discr1_Pkg; use Discr1_Pkg;

package Discr1 is

  procedure Proc (V : Variable_String_Array);

end Discr1;
