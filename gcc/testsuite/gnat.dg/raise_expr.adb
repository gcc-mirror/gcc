--  { dg-do compile }

procedure Raise_Expr is

   E : exception;

   type T is tagged limited null record;
   type TC is new T with null record;

   function F0 return Boolean is
   begin
       return raise E;
   end;

   function F return T'Class is
     TT : T;
   begin
      return raise E; -- Causes compile-time crash
   end F;

begin
   declare
      O : T'class  := F;
   begin
      null;
   end;
end Raise_Expr;
