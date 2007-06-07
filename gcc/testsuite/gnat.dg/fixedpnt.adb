--  { dg-do run }

procedure Fixedpnt is
   A : Duration := 1.0;
   B : Duration := Duration ((-1.0) * A);
begin   
   if B > 0.0 then 
      raise Constraint_Error;
   end if;
end;    
