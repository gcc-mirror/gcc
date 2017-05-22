-- { dg-do compile }
-- { dg-options "-O -gnatn -Winline" }

package body Limited_With5 is
  procedure Doit (Obj : Limited_With5_Pkg.T) is
  begin
    if Limited_With5_Pkg.Get_Expression_Support (Obj) > Sup_T'(100) then
      raise Program_Error;
    end if;
  end Doit;
end Limited_With5;
