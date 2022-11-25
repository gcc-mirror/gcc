package body Opt100_Pkg is

   function Func (R : Rec) return Integer is
   begin
      if R in Small_Rec then
         case R.K is
            when A => return 0;
            when B => return 1;
            when C => return 2;
            when others => raise Program_Error;
         end case;
      else
         return -1;
      end if;
   end;

end Opt100_Pkg;
