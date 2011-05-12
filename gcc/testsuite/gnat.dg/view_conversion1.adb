-- { dg-do run }
-- { dg-options "-gnatws" }

procedure View_Conversion1 is

   type Matrix is array (Integer range <>, Integer range <>) of Float;

   S1 : Matrix (-3 .. -2, 2 .. 3) := ((2.0, -1.0), (-1.0, 2.0));
   S2 : Matrix (1 .. 2, 1 .. 2) := S1;
   S3 : Matrix (2 .. 3, -3 .. -2);
   S4 : Matrix (1 .. 2, 1 .. 2);

   function Normal_Last (A : Matrix; N : Natural) return Boolean is
   begin
      if A'Last (1) = N and then A'Last (2) = N then
         return True;
      else
         return False;
      end if;
   end;

   procedure Transpose (A : Matrix; B : out Matrix) is
      N : constant Natural := A'Length (1);
      subtype Normal_Matrix is Matrix (1 .. N, 1 .. N);
   begin
      if not Normal_Last (A, N) or else not Normal_Last (B, N) then
         Transpose (Normal_Matrix (A), Normal_Matrix (B));
         return;
      end if;

      for J in 1 .. N loop
         for K in 1 .. N loop
            B (J, K) := A (K, J);
         end loop;
      end loop;
   end;

begin
   Transpose (S1, S3);
   Transpose (S3, S4);

   if S4 /= S2 then
      raise Program_Error;
   end if;
end;
