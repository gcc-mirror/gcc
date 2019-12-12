procedure Task5 is

   task type T is
      entry E (V1, V2 : Integer);
   end T;

   T_Obj : T;

   task body T is
      V1 : Integer;
      V2 : Integer;
      V3 : Integer;
   begin
      accept E (V1, V2 : Integer) do
         T.V1 := V1;
         T.V2 := V2;

         T_Obj.V1 := V1;  -- { dg-error "invalid reference to private operation of some object of type \"T\"" }
         T_Obj.V2 := V2;  -- { dg-error "invalid reference to private operation of some object of type \"T\"" }
         T_Obj.V3 := V3;  -- { dg-error "invalid reference to private operation of some object of type \"T\"" }
      end E;
   end T;

begin
   null;
end Task5;
