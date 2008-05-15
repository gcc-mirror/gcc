with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Discr8_Pkg2; use Discr8_Pkg2;

package Discr8_Pkg1 is

  type T is record
    A : Unbounded_String;
    B : L;
  end record;

end Discr8_Pkg1;
