-- { dg-do run }
-- { dg-options "-O -ftree-vrp -fno-inline" }

procedure Opt88 is

  Val : Integer := 1;

  procedure Dummy (B : out Boolean) is
  begin
    B := True;
  end;

  function Test return Boolean is
  begin
    return False;
  end;

  procedure Do_It (OK : out Boolean) is

    Blue : Boolean := False;
    Red  : Boolean := False;

  begin
    OK := True;
    Blue := True;
    Dummy (Red);

    if Red then
      Red := False;

      if Test then
        Dummy (Red);
      end if;
    end if;

    if Blue and not Red then
      Val := 0;
    end if;

    if Red then
      OK := False;
    end if;
  end;

  OK : Boolean;

begin
  Do_It (OK);
  if not OK then
    raise Program_Error;
  end if;
end;
