--  { dg-do compile }
procedure Protected_Self_Ref2 is

   protected type P is
      procedure Foo;
   end P;

   protected body P is
      procedure Foo is
	 D : Integer;
      begin
         D := P'Digits;  -- { dg-error "denotes current instance" }
      end;
   end P;

begin
   null;
end Protected_Self_Ref2;
