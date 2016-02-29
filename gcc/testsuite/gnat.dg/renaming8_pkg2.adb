package body Renaming8_Pkg2 is

  function F return Rec is
  begin
    return (E => (others => True));
  end;

end Renaming8_Pkg2;
