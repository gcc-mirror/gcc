--  { dg-do compile }
generic         
package Iface_Eq_Test is
   type Iface is limited interface;
   function "=" (L, R : access Iface) return Boolean is abstract;
end;               
