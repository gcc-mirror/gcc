with Varsize4_Pkg;

package Varsize4 is

   type Arr is array (1 .. Varsize4_Pkg.F) of Boolean;

   function Get return Natural;

end Varsize4;
