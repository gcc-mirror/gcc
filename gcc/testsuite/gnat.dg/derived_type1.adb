-- { dg-do compile }
-- { dg-options "-gnatws -fdump-tree-original" }

procedure Derived_Type1 is

  type Root is tagged null record;

  type Derived1 is new Root with record
    I1 : Integer;
  end record;

  type Derived2 is new Derived1 with record
    I2: Integer;
  end record;

  R : Root;
  D1 : Derived1;
  D2 : Derived2;

begin
  R  := Root(D1);
  R  := Root(D2);
  D1 := Derived1(D2);
end;

-- { dg-final { scan-tree-dump-not "VIEW_CONVERT_EXPR<struct derived_type1__root>" "original" } }
-- { dg-final { scan-tree-dump-not "VIEW_CONVERT_EXPR<struct derived_type1__derived1>" "original" } }
-- { dg-final { cleanup-tree-dump "original" } }
