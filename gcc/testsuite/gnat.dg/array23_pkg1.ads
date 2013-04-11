with Array23_Pkg2;

package Array23_Pkg1 is

   C2 : Natural := Array23_Pkg2.C1;

   subtype Index is Natural range 0 .. C2;

   type Inner is array (Index) of Natural;

   type Arr is array (Array23_Pkg2.Index) of Inner;

end Array23_Pkg1;
