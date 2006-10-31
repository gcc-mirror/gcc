-- { dg-do compile } 

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package Controller is
   type Iface is interface;
   type Thing is tagged record
      Name : Unbounded_String;
   end record;
   type Object is abstract new Thing and Iface with private;
private
   type Object is abstract new Thing  and Iface
   with record
      Surname : Unbounded_String;
   end record;
end Controller;
