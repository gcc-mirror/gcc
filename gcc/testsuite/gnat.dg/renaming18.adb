-- { dg-do compile }
-- { dg-options "-gnatwu" }

procedure Renaming18 is

  type T is record
    Item : Integer;
  end record;

  A_T : T;
  Item : Integer renames A_T.Item;

  type VFA_T is record
    Item : Integer;
  end record
  with Volatile_Full_Access;

  A_VFA_T : VFA_T;
  VFA_Item : Integer renames A_VFA_T.Item; -- { dg-bogus "is not referenced" }

begin
  Item := 42;
  VFA_Item := 42;
end;
