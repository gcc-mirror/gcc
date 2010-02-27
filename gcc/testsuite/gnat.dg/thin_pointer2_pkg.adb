package body Thin_Pointer2_Pkg is

   type SB is access constant String;

   function Inner (S : SB) return Character is
   begin
      if S /= null and then S'Length > 0 then
         return S (S'First);
      end if;
      return '*';
   end;

   function F return Character is
   begin
      return Inner (SB (S));
   end;

end Thin_Pointer2_Pkg;
