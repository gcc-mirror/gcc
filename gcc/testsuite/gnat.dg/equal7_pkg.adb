package body Equal7_Pkg is

   function F (X : Integer) return String is
   begin
      return To_String (F (X));
   end F;

   function F (X : Integer) return Unbounded_String is
      Result : Unbounded_String;
   begin
      Append (Result, "hello" & X'Img);
      return Result;
   end;
end;
