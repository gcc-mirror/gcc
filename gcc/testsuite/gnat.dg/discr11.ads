with Discr11_Pkg; use Discr11_Pkg;

package Discr11 is
   type DT_2 is new DT_1 with record
     More : Integer;
   end record;

   function Create return DT_2;
end Discr11;
