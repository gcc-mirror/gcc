--  { dg-do run }
--  { dg-options "-gnatws" }

pragma Assertion_Policy (Check);

procedure assert1 is
   Int128 : constant Boolean := Standard'Max_Integer_Size = 128;
   type p1 is array (1 .. 113) of Boolean;
   pragma Pack (p1);
   type p2 is array (1 .. 13) of Boolean;
   pragma Pack (p2);
   type p3 is array (1 .. 113) of Boolean;
   pragma Pack (p3);
   for p3'size use 113;
   type p4 is array (1 .. 13) of Boolean;
   pragma Pack (p4);
   for p4'size use 13;
   v1 : p1;
   v2 : p2;
   v3 : p3;
   v4 : p4;
begin
   pragma Assert (p1'Size = (if Int128 then 113 else 120));
   pragma Assert (p2'Size = 13);
   pragma Assert (p3'Size = 113);
   pragma Assert (p4'Size = 13);
   pragma Assert (p1'Value_Size = (if Int128 then 113 else 120));
   pragma Assert (p2'Value_Size = 13);
   pragma Assert (p3'Value_Size = 113);
   pragma Assert (p4'Value_Size = 13);
   pragma Assert (p1'Object_Size = (if Int128 then 128 else 120));
   pragma Assert (p2'Object_Size = 16);
   pragma Assert (p3'Object_Size = (if Int128 then 128 else 120));
   pragma Assert (p4'Object_Size = 16);
   pragma Assert (v1'Size = (if Int128 then 128 else 120));
   pragma Assert (v2'Size = 16);
   pragma Assert (v3'Size = (if Int128 then 128 else 120));
   pragma Assert (v4'Size = 16);
   null;
end;
