package body Discr42_Pkg is

   function F (Pos : in out Natural) return Rec is
   begin
      Pos := Pos + 1;
      if Pos > 1 then
        return (D => True, N => Pos * 2);
      else
        return (D => False);
      end if;
   end;

end Discr42_Pkg;
