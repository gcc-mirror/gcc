--  { dg-do compile }
--  { dg-options "-gnatc" }

pragma Restrictions (No_Entry_Queue);
package Restricted_Pkg is
   type Iface is limited interface;
   protected type PO is new Iface with
      procedure Dummy;
   end; 
end;    
