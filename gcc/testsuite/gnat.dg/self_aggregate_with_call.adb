-- { dg-do run }
-- { dg-options "-O2" }

procedure self_aggregate_with_call is

   type Values is array (1 .. 8) of Natural;

   type Vector is record
      Components : Values;
   end record;

   function Clone (Components: Values) return Values is
   begin
      return Components;
   end;

   procedure Process (V : in out Vector) is
   begin
      V.Components (Values'First) := 1;
      V := (Components => Clone (V.Components));

      if V.Components (Values'First) /= 1 then
         raise Program_Error;
      end if;
   end;

   V : Vector;
begin
   Process (V);
end;
