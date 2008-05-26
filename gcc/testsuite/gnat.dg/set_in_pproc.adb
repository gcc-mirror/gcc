--  { dg-do compile }

with Ada.Containers.Ordered_Sets;
procedure Set_In_Pproc is

   protected type Ptype is
      procedure Pproc;
   end;
   
   protected body Ptype is
      procedure Pproc is
         package Sets is
            new Ada.Containers.Ordered_Sets (Element_Type => Integer);
      begin
         null;
      end;
   end; 
begin   
   null;
end;    
