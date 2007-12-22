-- { dg-do compile }
-- { dg-options "-gnatws" }

procedure Pack2 is

   type Bits_T is record
      B0, B1, B2: Boolean;
   end record;

   type State_T is record
      Valid : Boolean;
      Value : Bits_T;
   end record;
   pragma Pack (State_T);
      
   procedure Process (Bits : Bits_T) is begin null; end;
   
   State : State_T;

begin
   Process (State.Value);
end;
