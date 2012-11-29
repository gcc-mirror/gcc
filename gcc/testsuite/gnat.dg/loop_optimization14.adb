-- PR middle-end/55321
-- { dg-do compile }
-- { dg-options "-O" }

with Loop_Optimization14_Pkg; use Loop_Optimization14_Pkg;

package body Loop_Optimization14 is

   procedure Finalize_Pool (Pool : in out Rec) is
      Raised : Boolean := False;
   begin
      Pool.A := True;

      while not Pool.B loop

         begin
            Proc (Pool.B);

         exception
            when others =>
               if not Raised then
                  Raised := True;
               end if;
         end;
      end loop;

   end;

end Loop_Optimization14;
