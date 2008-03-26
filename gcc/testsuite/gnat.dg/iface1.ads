generic
   type Data is private;
package Iface1 is
   type Future is synchronized interface;
   type Any_Future is access all Future;

   procedure Get (This : in out Future; P : out Data) is abstract;
   procedure Set (This : in out Future; P : in Data) is abstract;

   type Reusable_Future is synchronized interface and Future;
   type Any_Reusable_Future is access all Reusable_Future'Class;
end Iface1;
