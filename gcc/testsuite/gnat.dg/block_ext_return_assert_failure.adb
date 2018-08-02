--  { dg-do compile }

--  This test used to crash a compiler with assertions enabled

procedure Block_Ext_Return_Assert_Failure is

   function Return_Int return Integer is
   begin
      return 123;
   end Return_Int;

   function F return Integer is
   begin
      declare
      begin
         return Result : constant Integer := Return_Int do
            null;
         end return;
      end;
   end F;

begin
   null;
end Block_Ext_Return_Assert_Failure;
