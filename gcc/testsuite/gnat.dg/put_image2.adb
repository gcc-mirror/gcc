-- { dg-do run }
-- { dg-options "-gnat2022" }

procedure Put_Image2 is

  type T is array (1 .. 13) of Integer;

  function "&" (Left : T; Right : T) return T is (others => 2);

  function To_Virtual_String (Item : String) return T is (others => 0);

  procedure F (S : T) is null;

  X : array (1 .. 1) of Integer := [others => 0];

begin
  F ((others => 0) & To_Virtual_String (X'Image));
end;
