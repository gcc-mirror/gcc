-- { dg-do run }
-- { dg-options "-gnatws" }
-- { dg-options "-gnatws -flto" { target lto } }

pragma Locking_Policy (Ceiling_Locking);

with Lto8_Pkg; use Lto8_Pkg;

procedure Lto8 is
   task Tsk is
      pragma Priority (10);
   end Tsk;
   task body Tsk is
   begin
      Sema2.Seize;
      Sema1.Seize;
   exception
      when Program_Error => null;
   end;
begin
   null;
end;
