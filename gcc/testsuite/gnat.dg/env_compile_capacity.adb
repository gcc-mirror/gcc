-- { dg-do compile }

with My_Env_Versioned_Value_Set_G;
package body Env_Compile_Capacity is
  generic 
    with package Env_Obj_Set_Instance is
       new My_Env_Versioned_Value_Set_G(<>);
    with function Updated_Entity (Value : Env_Obj_Set_Instance.Value_T)
        return Boolean is <>;
    with package Entity_Upd_Iteration is
       new Env_Obj_Set_Instance.Update_G (Updated_Entity);
  procedure Compile_G;
  procedure Compile_G is begin null; end;
  package My_Env_Aerodrome is
     new My_Env_Versioned_Value_Set_G (Value_T => String);
  function Updated_Entity (Id : in String) return Boolean is
    begin return True; end;
  package Iteration_Aerodrome_Arrival is
     new My_Env_Aerodrome.Update_G (Updated_Entity);
  procedure Aerodrome_Arrival is new Compile_G
    (Env_Obj_Set_Instance  => My_Env_Aerodrome,
     Updated_Entity        => Updated_Entity,
     Entity_Upd_Iteration  => Iteration_Aerodrome_Arrival);
end Env_Compile_Capacity;
