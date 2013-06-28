package Incomplete3 is

   type Output_T;
   type Output_T is abstract tagged private;

   type Tracer_T is tagged private;

   function Get_Tracer (This : access Output_T'Class) return Tracer_T'class;

   function Get_Output (This : in Tracer_T) return access Output_T'Class;

private

   type Output_T is abstract tagged record
      B : Boolean := True;
   end record;

   type Tracer_T is tagged record
      Output : access Output_T'Class := null;
   end record;

end Incomplete3;
