-- { dg-do compile }

package body Discr46 is

   function F (Id : Enum) return Integer is
      Node : Integer := 0;
   begin
      if A (Id).R.D = True then
         Node := A (Id).R.T;
      end if;
      return Node;
   end; 

end Discr46;
