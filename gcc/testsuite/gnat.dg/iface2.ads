with Iface1;
generic
   with package Prot is new Iface1 (<>);
package Iface2 is
   procedure change (This, That : Prot.Any_Future);
end Iface2;
