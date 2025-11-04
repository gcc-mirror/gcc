-- { dg-do compile }

procedure Protected_Subtype1 is

   protected type Object with Lock_Free => True is
   end Object;

   protected body Object is
   end Object;

   A : Object;

   subtype Object_Subtype is Object;

   B : Object_Subtype;

   type Rec is record
      A : Object;         
      B : Object_Subtype;
   end record;

   C : Rec;

begin
  null;
end;
