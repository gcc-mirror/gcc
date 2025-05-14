with Ada.Strings.Bounded;

package Lto29_Pkg is

  package M is new Ada.Strings.Bounded.Generic_Bounded_Length (10);

  type T is new M.Bounded_String;

  Null_T : constant T;

private

  Null_T : constant T := To_Bounded_String ("");

end Lto29_Pkg;
