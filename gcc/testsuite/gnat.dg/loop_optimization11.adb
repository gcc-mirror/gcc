-- { dg-do compile }
-- { dg-options "-O" }

with Loop_Optimization11_Pkg; use Loop_Optimization11_Pkg;

procedure Loop_Optimization11 is
   Arr : array (Prot, Mem) of Integer := (others => (others => 0));
begin
   Put_Line (Img (0) & " ");
   for I in Arr'Range (1) loop
      for J in Arr'Range (2) loop
         declare
            Elem : Integer renames Arr (I, J);
         begin
            Put_Line (Img (Elem));
         end;
      end loop;
   end loop;
end;
