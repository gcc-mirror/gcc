-- { dg-do run }
-- { dg-options "-Os" }

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Opt41_Pkg;             use Opt41_Pkg;

procedure Opt41 is
   R  : Rec := (Five, To_Unbounded_String ("CONFIG"));
   SP : String_Access := new String'(To_String (Rec_Write (R)));
   RP : Rec_Ptr := new Rec'(Rec_Read (SP));
begin
   if RP.D /= R.D then
      raise Program_Error;
   end if;
end;
