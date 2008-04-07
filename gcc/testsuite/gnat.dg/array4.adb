-- { dg-do run }

procedure Array4 is

   type A is array (1..5) of Integer;
   f : constant A := (1, 2, 3, 4, 5);

   i1 : integer renames f(1);
   i2 : integer renames f(2);
   i3 : integer renames f(3);
   i4 : integer renames f(4);
   i5 : integer renames f(5);

   procedure Link_Failure;
   pragma Import (C, Link_Failure);

begin
  if i1 /= 1 then
    Link_Failure;
  end if;

  if i2 /= 2 then
    Link_Failure;
  end if;

  if i3 /= 3 then
    Link_Failure;
  end if;

  if i4 /= 4 then
    Link_Failure;
  end if;

  if i5 /= 5 then
    Link_Failure;
  end if;
end;
