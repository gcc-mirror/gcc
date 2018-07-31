--  { dg-do compile }
--  { dg-options "-gnatd.F -gnatwa" }

with Ada.Dispatching;

procedure Contract1 with SPARK_Mode is

   function Foo return Boolean is
   begin
      Ada.Dispatching.Yield;
      return True;
   end Foo;

   Dummy : constant Integer := 0;

begin
   if Foo and then True then
      raise Program_Error;
   end if;
end Contract1;
