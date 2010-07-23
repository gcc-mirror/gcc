-- { dg-do compile }

with Aggr16_Pkg; use Aggr16_Pkg;

package body Aggr16 is

  type Arr is array (1 .. 4) of Time;

  type Change_Type is (One, Two, Three);

  type Change (D : Change_Type) is record
    case D is
      when Three =>
        A : Arr;
      when Others =>
        B : Boolean;
    end case;
  end record;

  procedure Proc is
    C : Change (Three);
  begin
    C.A := (others => Null_Time);
  end;

end Aggr16;
