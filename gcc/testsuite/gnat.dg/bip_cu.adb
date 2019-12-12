--  { dg-do compile }

with BIP_CU_T; use BIP_CU_T;
with BIP_CU_Constructor;

procedure BIP_CU is
    Value : constant T := BIP_CU_Constructor;
begin
    null;
end;
