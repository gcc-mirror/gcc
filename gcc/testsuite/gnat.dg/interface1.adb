-- { dg-do run }

with System;
procedure Interface1 is
   package Pkg is
      type I1 is interface;
      type Root is tagged record
         Data : string (1 .. 300);
      end record;
      type DT is new Root and I1 with null record;
   end Pkg;
   use Pkg; 
   use type System.Address; 
   Obj : DT;
   procedure IW (O : I1'Class) is 
   begin
      if O'Address /= Obj'Address then
         raise Program_Error;
      end if;
   end IW;
begin   
   IW (Obj);
end Interface1;
