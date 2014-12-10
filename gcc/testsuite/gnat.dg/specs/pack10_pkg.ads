-- { dg-excess-errors "cannot generate code" }

package Pack10_Pkg is

   generic
      type Vector_Type (<>) is private;
   procedure Proc;

end Pack10_Pkg;
