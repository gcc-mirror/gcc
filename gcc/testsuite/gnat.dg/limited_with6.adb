-- { dg-do compile }
-- { dg-options "-O -gnatn -Winline" }

package body Limited_With6 is
  procedure Doit (Obj : Limited_With6_Pkg.T) is
  begin
    if Limited_With6_Pkg.Get_Expression_Support (Obj) > Sup_T'(100) then
      raise Program_Error;
    end if;
  end Doit;
end Limited_With6;
