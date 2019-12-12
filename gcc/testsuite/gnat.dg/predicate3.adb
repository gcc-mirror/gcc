--  { dg-do run }
--  { dg-options "-gnata" }

with Ada.Assertions, Ada.Text_IO;
use  Ada.Assertions, Ada.Text_IO;

with Predicate3_Pkg;
use  Predicate3_Pkg;

procedure Predicate3 is
   Got_Assertion : Boolean := False;
begin

   begin
      Put_Line (Good (C)'Image);
   exception
      when Assertion_Error =>
         Got_Assertion := True;
   end;

   if not Got_Assertion then
      raise Program_Error;
   end if;

   Got_Assertion := False;
   declare
      X: Priv;
   begin
      X := Wrong;
   exception
      when Assertion_Error =>
         Got_Assertion := True;
   end;

   if not Got_Assertion then
      raise Program_Error;
   end if;

end Predicate3;
