-- { dg-do run }
-- { dg-options "-gnatws -gnatVa" }

pragma Initialize_Scalars;

procedure Invalid1 is

  X : Boolean;
  A : Boolean := False;

  procedure Uninit (B : out Boolean) is
  begin
    if A then
      B := True;
      raise Program_Error;
    end if;
  end;

begin

  -- first, check that initialize_scalars is enabled
  begin
    if X then
      A := False;
    end if;
    raise Program_Error;
  exception
    when Constraint_Error => null;
  end;

  -- second, check if copyback of an invalid value raises constraint error
  begin
    Uninit (A);
    if A then
      -- we expect constraint error in the 'if' above according to gnat ug:
      -- ....
      -- call.  Note that there is no specific option to test `out'
      -- parameters, but any reference within the subprogram will be tested
      -- in the usual manner, and if an invalid value is copied back, any
      -- reference to it will be subject to validity checking.
      -- ...
      raise Program_Error;
    end if;
    raise Program_Error;
  exception
    when Constraint_Error => null;
  end;

end;
