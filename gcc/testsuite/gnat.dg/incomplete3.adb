-- { dg-do compile }

package body Incomplete3 is

   function Get_Tracer (This : access Output_T'Class) return Tracer_T'class is
   begin
      return Tracer_T'Class (Tracer_T'(Output => This));
   end ;

   function Get_Output (This : in Tracer_T) return access Output_T'Class is
   begin
      return This.Output;
   end;

end Incomplete3;
