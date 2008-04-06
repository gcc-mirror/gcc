-- { dg-do compile }

procedure Pack5 is

  type Kind is (v1, v2, v3);

  type Error (k : Kind := Kind'First) is record
    case k is
    when v1 =>
      null;
    when v2 =>
      null;
    when Others =>
      B : Boolean;
    end case;
  end record;
  pragma Pack (Error);
  for Error'Size use 16;

  No_Error: constant Error := (k => v2);

  type R (B : Boolean) is record
    E : Error;
  end record;
  pragma Pack(R);
  type Ptr is access R;

  C : Ptr := new R (True);

begin
  C.E := No_Error;
end;
