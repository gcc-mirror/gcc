package body Warn20_Pkg is
   L : array (1 .. 10) of T := (1 .. 10 => None);
   procedure Foo is
   begin
      for A of L loop
         exit when A = None;
         Dispatch (A);
      end loop;
   end;
end;
