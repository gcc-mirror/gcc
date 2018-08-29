--  { dg-do compile }
--  { dg-options "-gnatVi -gnatws" }

with Validity_Check2_Pkg; use Validity_Check2_Pkg;

procedure Validity_Check2 (R : access Rec) is
begin
  if Op_Code_To_Msg (R.Code) in Valid_Msg then
    raise Program_Error;
  end if;
end;
