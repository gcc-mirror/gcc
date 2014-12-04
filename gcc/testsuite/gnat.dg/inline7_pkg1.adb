with Inline7_Pkg2;

package body Inline7_Pkg1 is

  procedure Test (I : Integer) is

    function F is new Inline7_Pkg2.Calc (I);

  begin
    if I /= F (I) then
      raise Program_Error;
    end if;
  end;

end Inline7_Pkg1;
