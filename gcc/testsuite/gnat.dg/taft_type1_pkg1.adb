with Taft_Type1_Pkg2;

package body Taft_Type1_Pkg1 is
   type TAMT1 is new Taft_Type1_Pkg2.Priv (X => 1);
   type TAMT2 is new Taft_Type1_Pkg2.Priv;

   procedure Check is
      Ptr1 : TAMT1_Access := new TAMT1;
      Ptr2 : TAMT2_Access := new TAMT2 (X => 2);
   begin
      if Ptr1.all.X /= 1 then
         raise Program_Error;
      end if;
      if Ptr2.all.X /= 2 then
         raise Program_Error;
      end if;
   end;
end Taft_Type1_Pkg1;
