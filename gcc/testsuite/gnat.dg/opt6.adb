-- PR rtl-optimization/45394

-- { dg-do compile }
-- { dg-options "-O2 -g" }

package body Opt6 is

   function Current_Parameter (Iter : Command_Line_Iterator) return String is
   begin
      if Iter.Params = null
        or else Iter.Current > Iter.Params'Last
        or else Iter.Params (Iter.Current) = null
      then
         return "";

      else
         declare
            P : constant String := Iter.Params (Iter.Current).all;

         begin
            --  Skip separator

            return P (P'First + 1 .. P'Last);
         end;
      end if;
   end Current_Parameter;

end Opt6;
