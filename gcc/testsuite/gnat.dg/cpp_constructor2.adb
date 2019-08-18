--  { dg-do compile }

procedure CPP_Constructor2 is

   package P is
      type X is tagged limited record
         A, B, C, D : Integer;
      end record;
      pragma Import (Cpp, X);

      procedure F1 (V : X);
      pragma Import (Cpp, F1);

      function F2 return X; --  { dg-error "C\\+\\+ constructor must have external name or link name" }
      pragma Cpp_Constructor (F2);
   end P;
begin
  null;
end CPP_Constructor2;
