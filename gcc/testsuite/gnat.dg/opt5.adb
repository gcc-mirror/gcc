-- { dg-do run }
-- { dg-options "-O2" }

procedure Opt5 is

   type Varray is array (1 .. 4) of Natural;

   procedure Check_All_Ones (A : Varray) is
   begin
      for J in A'Range loop
         if (A (J)) /= 1 then
            raise Program_Error;
         end if;
      end loop;
   end;

   X : constant Varray := (1, 1, 1, 1);

begin
   Check_All_Ones (X);
end;
