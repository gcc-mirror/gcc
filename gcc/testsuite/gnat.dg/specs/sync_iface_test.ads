--  { dg-do compile }
--  { dg-options "-gnatc" }

package Sync_Iface_Test is
   type Iface is limited interface;
   procedure Do_Test
     (Container : in out Iface;
      Process   : access procedure (E : Natural)) is abstract;
   
   protected type Buffer is new Iface with
      overriding procedure Do_Test
        (Process : access procedure (E : Natural));
   end;
end;
