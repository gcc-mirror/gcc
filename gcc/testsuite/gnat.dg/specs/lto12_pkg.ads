-- { dg-excess-errors "cannot generate code" }

package Lto12_Pkg is

  type R (Kind  : Boolean := False) is record
    case Kind is
      when True => I : Integer;
      when others => null;
    end case;
  end record;

  function F return R;

end Lto12_Pkg;
