-- { dg-do compile }
-- { dg-options "-gnatws" }

package body Aggr15 is

  function CREATE return DATA_T is
    D : DATA_T;
  begin
    return D;
  end;

  function ALL_CREATE return ALL_DATA_T is
    C : constant ALL_DATA_T := (others => (others => Create));
  begin
    return C;
  end;

end Aggr15;
