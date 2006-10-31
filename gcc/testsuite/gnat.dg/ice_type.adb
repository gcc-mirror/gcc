-- { dg-do compile }

with ICE_Types; use ICE_Types;
procedure ICE_Type is
   type Local_Float_T is new Float_View_T;
   LF : Local_Float_T;
begin
   Initialize (Float_View_T (LF));
end;
