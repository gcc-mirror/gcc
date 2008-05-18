with System;

package Loop_Optimization2 is

   type Prim_Ptr is access procedure;
   type Address_Array is array (Positive range <>) of Prim_Ptr;

   subtype Dispatch_Table is Address_Array (1 .. 1);

   type Tag is access all Dispatch_Table;

   type Tag_Array is array (Positive range <>) of Tag;

   function Interface_Ancestor_Tags (T : Tag) return Tag_Array;

   type Interface_Data_Element is record
      Iface_Tag : Tag;
   end record;

   type Interfaces_Array is array (Natural range <>) of Interface_Data_Element;

   type Interface_Data (Nb_Ifaces : Positive) is record
      Ifaces_Table : Interfaces_Array (1 .. Nb_Ifaces);
   end record;

   type Interface_Data_Ptr is access all Interface_Data;

   type Type_Specific_Data (Idepth : Natural) is record
      Interfaces_Table : Interface_Data_Ptr;
   end record;

   type Type_Specific_Data_Ptr is access all Type_Specific_Data;
   pragma No_Strict_Aliasing (Type_Specific_Data_Ptr);

   subtype Predef_Prims_Table is Address_Array (1 .. 16);
   type Predef_Prims_Table_Ptr is access Predef_Prims_Table;

   type Addr_Ptr is access System.Address;
   pragma No_Strict_Aliasing (Addr_Ptr);

end Loop_Optimization2;
