pragma Restrictions (No_Elaboration_Code);

package body Atomic7_Pkg2 is

  T : Natural := 0;
  pragma Atomic (T);

  function Stamp return Natural is
  begin
     T := T + 1;
     return T;
  end;

end Atomic7_Pkg2;
