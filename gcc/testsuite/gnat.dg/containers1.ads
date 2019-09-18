with Ada.Containers.Bounded_Ordered_Sets; use Ada.Containers;
package Containers1 is
   pragma Suppress (All_Checks);
   package Sets is new Bounded_Ordered_Sets (Boolean);
   procedure Dummy;
end Containers1;