--  { dg-do compile }
--  { dg-options "-gnatwa" }

package body Warn28 is

   function Id (X : Integer) return Integer is (2 * X);

   procedure TheProcedure1 (TheParameter : in Integer)
   is
   X : Integer;
   begin

      X := Id (TheParameter);
      if X < 3 then
         X := X ** 3;
      end if;
   end TheProcedure1;

   procedure Junk (It : Integer) is  --  { dg-warning "formal parameter \"It\" is not referenced" }
      X : Integer := Id (34);
   begin
      if X < 3 then
         X := X ** 3;
      end if;
   end;

   procedure TheProcedure (TheParameter : in Integer)  --  { dg-warning "formal parameter \"TheParameter\" is not referenced" }
   is

   begin

      null;

   end TheProcedure;

end Warn28;
