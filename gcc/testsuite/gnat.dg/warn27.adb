--  { dg-do compile }

procedure Warn27 is
   Dummy : Boolean;

   pragma Compile_Time_Warning (Dummy, "warning");  --  { dg-warning "condition is not known at compile time" }
   pragma Compile_Time_Error (Dummy, "error");  --  { dg-warning "condition is not known at compile time" }
begin
   pragma Unreferenced (Dummy);
end Warn27;
