with Inline8_Pkg2;

package body Inline8_Pkg1 is

  procedure Test (I : Integer) is

    function F is new Inline8_Pkg2.Calc (I);
    pragma Inline (F);

  begin
    if I /= F (I) then
      raise Program_Error;
    end if;
  end;

end Inline8_Pkg1;
