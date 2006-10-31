-- { dg-do compile }

with gen_interface_p;
package gen_interface is
   type T is interface;
   procedure P (Thing: T) is abstract;
   package NG is new gen_interface_p (T, P);
end;    
