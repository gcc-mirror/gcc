-- { dg-do compile }

with equal1;
procedure test_equal1 is
  subtype Boolean_T is Boolean;
  function "=" (L, R : in equal1.Basic_Connection_Status_T)
    return Boolean_T renames equal1."=";
  Status : equal1.Basic_Connection_Status_T;
  Result : Boolean_T;
begin
  Status := equal1.Temporary_Disconnected;
  Result := Status /= equal1.Connected;
end;
