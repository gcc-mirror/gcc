-- { dg-do compile }

with Volatile5_Pkg; use Volatile5_Pkg;

procedure Volatile5 is

   A : Rec;

   procedure Proc is
   begin
      A := F;
   end;

begin
   Proc;
end;
