-- { dg-do run }

procedure too_many_tasks is 
   Global : Natural := 0;
   function Output return Integer is
   begin
      Global := Global + 1;
      return Global;
   end Output;
        
   task type A;
   task type B;
        
   task body A is
      I : Integer := Output;
      T : B;
   begin null; end A;
        
   task body B is
      I : Integer := Output;
      T : A;
   begin null; end B; 
        
   T : A;
begin null; end;
