-- { dg-do run }

with Ada.Text_IO;

procedure Protected_Deref1 is

   protected type Fallback_Hit_Counter_Type is
      procedure Handler;
   end Fallback_Hit_Counter_Type;

   protected body Fallback_Hit_Counter_Type is
      procedure Handler is
      begin
         Ada.Text_IO.Put_Line ("Test");
      end Handler;
   end Fallback_Hit_Counter_Type;

   Fallback_Hit_Counter : access Fallback_Hit_Counter_Type :=
     new Fallback_Hit_Counter_Type;

   type X is access protected procedure;

   A : X := Fallback_Hit_Counter.all.Handler'Access;
   B : X := Fallback_Hit_Counter.Handler'Access;

begin
   A.all;
   B.all;
   if A /= B then
      raise Program_Error;
   end if;
end;
