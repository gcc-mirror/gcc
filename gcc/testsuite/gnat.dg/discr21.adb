-- { dg-do compile }
-- { dg-options "-gnatws -O3" }

with Discr21_Pkg; use Discr21_Pkg;

package body Discr21 is

  type Index is new Natural range 0 .. 100;

  type Arr is array (Index range <> ) of Position;

  type Rec(Size : Index := 1) is record
    A : Arr(1 .. Size);
  end record;

  Data : Rec;

  function To_V(pos : Position) return VPosition is
  begin
    return To_Position(pos.x, pos.y, pos.z);
  end;

  procedure Read(Data : Rec) is
    pos : VPosition := To_V (Data.A(1));
  begin
    null;
  end;

  procedure Test is
  begin
    Read (Data);
  end;

end Discr21;
