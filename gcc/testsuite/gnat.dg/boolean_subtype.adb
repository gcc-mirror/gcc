-- { dg-do compile }
-- { dg-options "-O2" }

procedure boolean_subtype is

   subtype Component_T is Boolean;

   function Condition return Boolean is
   begin
      return True;
   end;

   V : Integer := 0;

   function Component_Value return Integer is
   begin
      V := V + 1;
      return V;
   end;

   Most_Significant  : Component_T := False;
   Least_Significant : Component_T := True;

begin

   if Condition then
      Most_Significant := True;
   end if;

   if Condition then
      Least_Significant := Component_T'Val (Component_Value);
   end if;

   if Least_Significant < Most_Significant then
      Least_Significant := Most_Significant;
   end if;

   if Least_Significant /= True then
      raise Program_Error;
   end if;

end;
