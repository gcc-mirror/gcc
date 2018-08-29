--  { dg-do compile }
--  { dg-options "-gnatwa" }

procedure Warn14 is

  type E is record
    P : Boolean;
  end record;

  EE : Boolean := True; --  { dg-warning "variable \"EE\" is not referenced" }

  function F1 (I : Natural) return Natural is --  { dg-warning "function \"F1\" is not referenced" }
  begin
    return I;
  end;

  function F2 (I : Natural) return Natural is (I); --  { dg-warning "function \"F2\" is not referenced" }

  function F3 (I : Natural) return Natural is (1); --  { dg-warning "function \"F3\" is not referenced|formal parameter \"I\" is not referenced" }

  function F7 (EE : E) return Boolean is (EE.P); --  { dg-warning "function \"F7\" is not referenced" }

  package YY is
    type XX is tagged null record;

    function F4 (Y : XX; U : Boolean) return Natural is (1); --  { dg-warning "formal parameter \"U\" is not referenced" }
  end YY;

  XXX : YY.XX;
  B : Natural := XXX.F4 (True); --  { dg-warning "variable \"B\" is not referenced" }
begin
  null;
end;
