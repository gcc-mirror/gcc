with Varsize3_Pkg2;
with Varsize3_Pkg3;

package Varsize3_Pkg1 is

   type Arr is array (Positive range 1 .. Varsize3_Pkg2.Last_Index) of Boolean;

   package My_G is new Varsize3_Pkg3 (Arr);

   type Object is new My_G.Object;

end Varsize3_Pkg1;
