-- { dg-do compile }
-- { dg-options "-O -gnatp" }

with Loop_Optimization5_Pkg; use Loop_Optimization5_Pkg;

procedure Loop_Optimization5 is
   Str : constant String := "12345678";
   Cmd : constant String := Init;
   StartP : Positive := Cmd'First;
   StartS : Positive := Cmd'Last + 1;
   EndP   : Natural := StartP - 1;
   Full_Cmd : String_Access;
begin
   for J in StartP .. Cmd'Last - Str'Length + 1 loop
      if Cmd (J .. J + Str'Length - 1) = Str then
         EndP := J - 1;
         exit;
      end if;
   end loop;
   Full_Cmd := Locate (Cmd (StartP .. EndP));
end;
