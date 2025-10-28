with Ada.Containers.Ordered_Sets;

generic
   with package Sets is new Ada.Containers.Ordered_Sets (<>);
package Generic_Inst5_Pkg1 is
   generic
   package Nested is
   end Nested;
end Generic_Inst5_Pkg1;
