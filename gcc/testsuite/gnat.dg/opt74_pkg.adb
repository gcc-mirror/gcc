package body Opt74_Pkg is

   procedure Proc (Found : out Integer; Index : out Integer) is
   begin
      Index := 1;
      Found := 0;
      while (Index <= A'Last) and (Found = 0) loop
         if A (Index) = 2 then
            Found := 1;
         else
            Index := Index + 1;
         end if;
      end loop;
   end;

end Opt74_Pkg;
