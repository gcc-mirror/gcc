package Volatile13 is

   type Index_Map (Length : Natural) is record
      Map : String (1 .. Length);
   end record;

   type Index_Map_Access is access all Index_Map;
   pragma Volatile (Index_Map_Access);

   type Shared_String (Size : Natural) is limited record
      Length    : Natural := 0;
      Index_Map : Index_Map_Access := null;
   end record;

   Shared_Empty : Shared_String := (Size => 64, others => <>);

   procedure Compute_Index_Map (Self : Shared_String);

end Volatile13;
