--  { dg-do compile }
--  { dg-options "-gnata" }

package body Loop_Invariant1 is

   procedure Proc (A : Arr; N : Integer) is
      I : Integer := A'First;
   begin
      while i <= A'Last and then A(A'First .. A'Last) /= A loop
         pragma Loop_Invariant (N = N'Loop_Entry);
         i := i + 1;
      end loop;
   end;

end Loop_Invariant1;
