-- { dg-do compile }
-- { dg-options "-O -gnatws" }

package body Opt104 is

   procedure Proc (R : Rec) is
      Data : Packed_Rec;

   begin
      case R.D is
         when True =>
            for I in 1 .. R.Len loop
               exit;
            end loop;

         when False =>
            null;
      end case;
   end;

end Opt104;

