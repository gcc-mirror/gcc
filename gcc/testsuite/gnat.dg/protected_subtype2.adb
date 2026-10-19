-- { dg-do compile }

with Ada.Unchecked_Deallocation;

procedure Protected_Subtype2 is

   protected type State (Capacity : Positive) with Lock_Free => False is
      procedure Touch;
   private
      Value : Natural := 0;
   end State;

   protected body State is
      procedure Touch is
      begin
         Value := Value + 1;
      end Touch;
   end State;

   subtype Constrained_State is State (4);
   type State_Access is access Constrained_State;

   procedure Free is
     new Ada.Unchecked_Deallocation (Constrained_State, State_Access);

begin
   for I in 1 .. 100 loop
      declare
         P : State_Access := new Constrained_State;
      begin
         P.Touch;
         Free (P);
      end;
   end loop;
end;

-- { dg-final { scan-assembler "finalize_protection" } }
