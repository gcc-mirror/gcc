with Atomic7_Pkg2; use Atomic7_Pkg2;

package Atomic7_Pkg1 is

  I : Integer := Stamp;
  pragma Atomic (I);

  J : Integer := Stamp;

end Atomic7_Pkg1;
