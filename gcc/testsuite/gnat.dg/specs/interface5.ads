--  { dg-do compile }
--  { dg-options "-gnatc" }

package interface5 is
   type Lim_Iface is limited interface;
   protected type Prot_Typ is new Lim_Iface with
   private
   end Prot_Typ;
end interface5;
