--  { dg-do run }
--  { dg-options "-gnata" }

procedure Predicate1 with SPARK_Mode is
    type R is record
       F : Integer;
    end record;

    package Nested is
       subtype S is R with Predicate => S.F = 42;
       procedure P (X : in out S) is null;

       type T is private;
       procedure P (X : in out T) is null;
    private
       type T is new S;
    end Nested;

    X : Nested.T;
    Y : Nested.S;

    X_Uninitialized : Boolean := False;
    Y_Uninitialized : Boolean := False;
begin
   begin
      Nested.P (X);
   exception
      when others => X_Uninitialized := True;
   end;

   begin
      Nested.P (Y);
   exception
      when others => Y_Uninitialized := True;
   end;

   if not X_Uninitialized or else not Y_Uninitialized then
      raise Program_Error;
   end if;
end Predicate1;
