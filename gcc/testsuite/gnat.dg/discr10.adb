package body Discr10 is

   function Get (X : R) return R is
   begin
     return R'(D1 => False, D2 => False, D3 => X.D3);
   end;

end Discr10;
