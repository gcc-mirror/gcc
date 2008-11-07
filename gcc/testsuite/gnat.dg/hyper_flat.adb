-- { dg-do run }
-- { dg-options "-gnatp" }

procedure Hyper_Flat is

   type Unsigned is mod 2 ** 32;
   x : Integer := 0;
   pragma Volatile (X);

   S : constant String := (1 .. X - 3 => 'A');
   --  Hyper-flat null string

begin
   if Unsigned'(S'Length) /= 0 then
      raise Program_Error;
   end if;
end;
