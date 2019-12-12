-- { dg-do run }

with Max_Size_Pkg; use Max_Size_Pkg;

procedure Max_Size is
begin
  if Arr1'Max_Size_In_Storage_Elements /= 7 then
    raise Program_Error;
  end if;
  if Arr2'Max_Size_In_Storage_Elements /= 24 then
    raise Program_Error;
  end if;
end;
