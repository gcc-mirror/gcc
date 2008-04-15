-- { dg-do compile }
-- { dg-options "-O3" }

package body Loop_Optimization1 is

   procedure Create (A : in out D; Val : Integer) is

      M : constant Group_Chain_List := Group_Chains(Val);
      G : constant Group_List := Groups(Val);

      function Is_Visible (Group : Number) return Boolean is
      begin
         for I in M'Range loop
            if Group = M(I).Groups(M(I).Length) then
               return True;
            end if;
         end loop;
         return False;
      end;

   begin
      for I in A.L'Range loop
         A.L(I) := new R(Is_Visible(G(I)));
      end loop;
   end;

end Loop_Optimization1;
