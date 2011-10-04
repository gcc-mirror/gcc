-- { dg-do run }
-- { dg-options "-O" }

with Opt22_Pkg; use Opt22_Pkg;

procedure Opt22 is

   procedure Go (S : String) is
   begin
      begin
        Fail;
      exception
        when Constraint_Error => Put ("the " & S);
      end;
      Put ("the " & S);
   end;

begin
   Go ("message");
end;
