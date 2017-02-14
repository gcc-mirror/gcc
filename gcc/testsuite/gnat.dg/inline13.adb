-- { dg-do compile }
-- { dg-options "-O -gnatn" }

package body Inline13 is

  function F (L : Arr) return String is
    Local : Arr (1 .. L'Length);
    Ret : String (1 .. L'Length);
    Pos : Natural := 1;
  begin
    Local (1 .. L'Length) := L;
    for I in 1 .. Integer (L'Length) loop
       Ret (Pos .. Pos + 8) := " " & Inline13_Pkg.Padded (Local (I));
       Pos := Pos + 9;
    end loop;
    return Ret;
  end;

end Inline13;
