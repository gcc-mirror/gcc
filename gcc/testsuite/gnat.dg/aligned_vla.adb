--  { dg-do run }

procedure Aligned_Vla is

   type Table is array (Integer range <>) of Integer;
   for Table'Alignment use Long_Float'Alignment;

   K : constant := 1;
   Konstants : Table (1 .. 4) := (others => K);

   procedure Check_Copy (Len : Integer) is
      My_Konstants : Table (1 .. Len) := Konstants (1 .. 1 + Len - 1);
   begin
      for I in My_Konstants'Range loop
         if My_Konstants (I) /= K then
            raise Program_Error;
         end if;
      end loop;
   end;

begin
   Check_Copy (Len => 4);
end;
