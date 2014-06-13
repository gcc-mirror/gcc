package body Opt38_Pkg is

  procedure Proc (I : Integer);
  pragma Inline (Proc);

  procedure Proc (I : Integer) is

    procedure Inner;
    pragma No_Inline (Inner);

    procedure Inner is
    begin
      if I /= 110 then
        raise Program_Error;
      end if;
    end;

  begin
    if I > 0 then
      Inner;
    end if;
  end;

  procedure Test (I : Integer) is
  begin
    if I > -1 then
      Proc (I);
    else
      Proc (I + 111);
    end if;
  end;

end Opt38_Pkg;
