-- { dg-do compile { target { lp64 || llp64 } } }
-- { dg-options "-O2 -gnatws" }

procedure Main is

   subtype Int64 is Long_Long_Integer;

   type Arr is array (Int64 range <>) of Boolean;

   Pow : constant := 10;

   procedure Compute (B : in out Arr) is
      Factor : Int64 := 3;
      Num    : Int64;
   begin
      while Factor <= 10 ** (Pow / 2) loop
         Num := Factor;
         while Num < 10 ** Pow loop
            if B (Num) then
               Factor := Num;
               exit;
            end if;
            Num := Num + 2;
         end loop;
         Num := Factor * Factor;
         while Num < 10 ** Pow loop
            B (Num) := False;
            Num        := Num + Factor * 2;
         end loop;
         Factor := Factor + 2;
      end loop;
   end;

   B : Arr (1 .. 10 ** Pow) := (others => True);

begin
   Compute (B);   
end;
