-- { dg-do run }
-- { dg-options "-O" }

procedure Opt30 is

   function Id_I (I : Integer) return Integer is
   begin
      return I;
   end;

   A : array (Integer range -4..4) of Integer;

begin
   A := (-ID_I(4), -ID_I(3), -ID_I(2), -ID_I(1), ID_I(100),
          ID_I(1), ID_I(2), ID_I(3), ID_I(4));
   A(-4..0) := A(0..4);
   if A /= (100, 1, 2, 3, 4, 1, 2, 3, 4) then
      raise Program_Error;
   end if;
end;
