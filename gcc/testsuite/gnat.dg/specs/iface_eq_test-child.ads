--  { dg-do compile }
--  { dg-options "-gnatc" }
generic                    
package Iface_Eq_Test.Child is
   protected type PO is new Iface with
      procedure Dummy;
   end;
   overriding function "=" (L, R : access PO) return Boolean;
end;      
