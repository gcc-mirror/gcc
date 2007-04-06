-- { dg-do run }

pragma Restrictions (No_Finalization);
procedure no_final is
   package P is
      type T is tagged null record;
      type T1 is new T with record
         A : String (1..80);
      end record;
      function F return T'Class;
   end P;
   
   Str : String (1..80) := (1..80=>'x');
   
   package body P is
      function F return T'Class is
         X : T1 := T1'(A => Str);
      begin
         return X;
      end F;
   end P;
   
   Obj : P.T'class := P.F;
begin
   if P.T1 (Obj).A /= Str then
      raise Constraint_Error;
   end if;
end;

