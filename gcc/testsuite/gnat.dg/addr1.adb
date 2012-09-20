--  { dg-do compile }

with System;
package body addr1 is
   task type T is
      entry Send (Location : System.Address);
   end;
   task body T is
   begin
      accept Send (Location : System.Address) do
        declare
           Buffer : String (1 .. 100);
           for Buffer'Address use Location;  --  Test
        begin
           null;
        end;
     end Send;
   end;
end;
