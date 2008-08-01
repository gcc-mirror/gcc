package body raise_from_pure is
   function Raise_CE_If_0 (P : Integer) return Integer is
   begin
      if P = 0 then
         raise Constraint_error;
      end if;
      return 1;
   end;
end;


