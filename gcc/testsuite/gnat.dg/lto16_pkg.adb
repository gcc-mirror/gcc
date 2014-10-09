with Ada.Calendar; use Ada.Calendar;

package body Lto16_Pkg is

  function F return Float is
    F1 : Float := Float (Seconds (Clock));
    F2 : Float := Float (Seconds (Clock));
    F  : Float;
  begin
    if F1 > F2 then
      F := (F2 - F1) / 2.0;
    else
      F := (F1 - F2) / 2.0;
    end if;
    return F;
  end;

end Lto16_Pkg;
