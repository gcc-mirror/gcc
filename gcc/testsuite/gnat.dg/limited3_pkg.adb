package body Limited3_Pkg is
   function F (I : Integer) return Rec is
   begin
      return (D => False, I => I);
   end;

   function FS (X : Integer) return Var_Rec is
   begin
      return (X, (1..X => '?'), Tag => <>);
   end FS;

   function F2 (I : Integer) return Rec2 is
   begin
      if I > 0 then
         return (D => False, I => I);
      else
         return (D => True, L => new Limited_Rec);
     end if;
   end;
end Limited3_Pkg;
