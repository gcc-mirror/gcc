-- { dg-do compile }

with pointer_protected_p;

procedure pointer_protected is
   Pointer : pointer_protected_p.Ptr := null;
   Data    : pointer_protected_p.T;
begin
   Pointer.all (Data);
end pointer_protected;
