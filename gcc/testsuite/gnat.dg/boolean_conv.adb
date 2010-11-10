-- { dg-do run }

with System; use System;

procedure Boolean_Conv is
  subtype B1 is Boolean;
  subtype B2 is Boolean;
  A0, A1, A2 : Address;

  B : aliased B1;

  procedure P2 (X2 : access B2) is
  begin
     A2 := X2.all'Address;
  end P2;

  procedure P1 (X1 : access B1) is
  begin
     A1 := X1.all'Address;
     P2 (B2 (X1.all)'Unrestricted_Access);
  end P1;

begin
   A0 := B'Address;
   P1 (B'Access);

   if A1 /= A0 then
      raise Program_Error;
   end if;

   if A2 /= A0 then
      raise Program_Error;
   end if;
end;
