--  { dg-do run }

with BIP_Prim_Func2_Pkg;

procedure BIP_Prim_Func2 is

   package B is
      type Instance is limited interface;
      function Make return Instance is abstract;
   end B;

   package C is
      type Instance is new B.Instance with null record;
      function Make return Instance is (null record);
   end C;

   package T is new BIP_Prim_Func2_Pkg (B.Instance, C.Instance, C.Make);

   Thing : B.Instance'Class := T.Make (2);

begin
   null;
end;
