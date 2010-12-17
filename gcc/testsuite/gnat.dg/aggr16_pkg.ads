package Aggr16_Pkg is

  type Time_Type is (A, B);

  type Time (D : Time_Type := A) is private;

  Null_Time : constant Time;

private

  type Hour is record
    I1 : Integer;
    I2 : Integer;
  end record;

  type Time (D : Time_Type := A) is record
    case D is
      when A =>
        A_Time : Integer;
      when B =>
        B_Time : Hour;
    end case;
  end record;

  Null_Time : constant Time := (A, 0);

end Aggr16_Pkg;
