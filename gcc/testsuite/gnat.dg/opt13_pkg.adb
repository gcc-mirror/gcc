package body Opt13_Pkg is

  subtype Index_Type is Natural range 0 .. 16;

  type Arr is array (Index_Type range <>) of Integer;

  type Rec is record
    F1, F2, F3 : Float;
    N : Natural;
    B1, B2 : Boolean;
    F4 : Float;
  end record;

  type Data (D : Index_Type) is record
    A : Arr (1 .. D);
    R : Rec;
  end record;

  Zero : constant Rec := (0.0, 0.0, 0.0, 0, False, False, 0.0);

  procedure Allocate (T : out My_Type) is
  begin
    T := new Data (Index_Type'last);
    T.R := Zero;

    for I in 1 .. T.A'last loop
      N := 1;
    end loop;
  end;

end Opt13_Pkg;
