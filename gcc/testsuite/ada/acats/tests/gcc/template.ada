with Report; use Report;

procedure Template is
begin   
   --  Test header
   Test ("TEMPLATE", "Template test for GNU Ada test suite");

   begin
      --  Body of test
      --  Call procedure Failed when detecting a failure
      Failed ("Pretend this test failed");
   end;

   --  Display result
   Result;
end Template;
