--  { dg-do compile }

package body Global
  with Refined_State => (State => Constit)
is
   Constit : Integer := 123;

   protected body Prot_Typ is
      procedure Force_Body is null;

      procedure Aspect_On_Spec
        with Global => (Input => Constit);
      procedure Aspect_On_Spec is null;

      procedure Aspect_On_Body
        with Global => (Input => Constit)
      is begin null; end Aspect_On_Body;

      procedure Pragma_On_Spec;
      pragma Global ((Input => Constit));
      procedure Pragma_On_Spec is null;

      procedure Pragma_On_Body is
         pragma Global ((Input => Constit));
      begin null; end Pragma_On_Body;
   end Prot_Typ;

   protected body Prot_Obj is
      procedure Force_Body is null;

      procedure Aspect_On_Spec
        with Global => (Input => Constit);
      procedure Aspect_On_Spec is null;

      procedure Aspect_On_Body
        with Global => (Input => Constit)
      is begin null; end Aspect_On_Body;

      procedure Pragma_On_Spec;
      pragma Global ((Input => Constit));
      procedure Pragma_On_Spec is null;

      procedure Pragma_On_Body is
         pragma Global ((Input => Constit));
      begin null; end Pragma_On_Body;
   end Prot_Obj;

   task body Task_Typ is
      procedure Aspect_On_Spec
        with Global => (Input => Constit);
      procedure Aspect_On_Spec is null;

      procedure Aspect_On_Body
        with Global => (Input => Constit)
      is begin null; end Aspect_On_Body;

      procedure Pragma_On_Spec;
      pragma Global ((Input => Constit));
      procedure Pragma_On_Spec is null;

      procedure Pragma_On_Body is
         pragma Global ((Input => Constit));
      begin null; end Pragma_On_Body;
   begin
      accept Force_Body;
   end Task_Typ;

   task body Task_Obj is
      procedure Aspect_On_Spec
        with Global => (Input => Constit);
      procedure Aspect_On_Spec is null;

      procedure Aspect_On_Body
        with Global => (Input => Constit)
      is begin null; end Aspect_On_Body;

      procedure Pragma_On_Spec;
      pragma Global ((Input => Constit));
      procedure Pragma_On_Spec is null;

      procedure Pragma_On_Body is
         pragma Global ((Input => Constit));
      begin null; end Pragma_On_Body;
   begin
      accept Force_Body;
   end Task_Obj;
end Global;
