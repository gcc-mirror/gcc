--  { dg-do run }
with System;

procedure Protected_Self_Ref1 is

   protected type P is
      procedure Foo;
   end P;

   protected body P is
      procedure Foo is
         Ptr : access P;  -- here P denotes the type P
	 T   : Integer;
	 A   : System.Address;
      begin
         Ptr := P'Access; -- here P denotes the "this" instance of P
	 T := P'Size;
	 A := P'Address;
      end;
   end P;

   O : P;
begin
   O.Foo;
end Protected_Self_Ref1;
