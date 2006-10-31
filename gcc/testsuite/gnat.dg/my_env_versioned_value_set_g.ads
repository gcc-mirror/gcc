generic 
  type Value_T(<>) is private;
package My_Env_Versioned_Value_Set_G is
  generic 
    with function Updated_Entity (Value : Value_T) return Boolean is <>;
  package Update_G is end; 
end;    
