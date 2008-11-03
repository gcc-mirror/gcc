-- { dg-do run }
-- { dg-options "-O" }

procedure Array5 is

    type myint is range 0 .. 100_000;
    Bla : constant myint := 359;

    type my_array is array (1 .. 2) of myint;

    type item is record
       Length  : Integer;
       Content : my_array;
    end record;

    procedure create_item (M : out item) is
    begin
      M.Length := 1;
      M.Content := (others => Bla);
    end;

    Var : item;

begin
    create_item (Var);

    if Var.Length = 1
     and then Var.Content (1) = Bla
    then
      null;
    else
      raise Program_Error;
    end if;
end;
