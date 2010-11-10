-- { dg-do compile }
-- { dg-options "-O2" }

with Opt10_Pkg; use Opt10_Pkg;

procedure Opt10 is

   procedure Compare_Rep_Data (MA, MB : Rep_Message) is
   begin
      if MA.Data /= MB.Data then
         raise Program_Error;
      end if;
   end;

   procedure Check_Rep_For (Bit : Boolean) is
      MA, MB : Rep_Message;
   begin
      Safe_Assign (MA, Bit);
      Safe_Assign (MB, Bit);
      Compare_Rep_Data (MA, MB);
   end;

begin
   Check_Rep_For (Bit => False);
end;

