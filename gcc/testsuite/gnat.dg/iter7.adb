--  { dg-do run }
--  { dg-options "-gnat2022" }

procedure Iter7 is

   type Enum is (A, B, C);
   for Enum use (A => 1, B => 2, C => 3);

   Enum_Filter : array (Enum) of Boolean := (others => False);

begin
   for F in Enum when Enum_Filter (F) loop
      raise Program_Error;
   end loop;
end;
