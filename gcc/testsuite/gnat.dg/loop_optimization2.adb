-- { dg-do compile }
-- { dg-options "-gnata -O2 -fno-inline" }

with Ada.Unchecked_Conversion;

package body Loop_Optimization2 is

   function To_Addr_Ptr is
      new Ada.Unchecked_Conversion (System.Address, Addr_Ptr);

   function To_Address is
     new Ada.Unchecked_Conversion (Tag, System.Address);

   function To_Type_Specific_Data_Ptr is
     new Ada.Unchecked_Conversion (System.Address, Type_Specific_Data_Ptr);

   function Interface_Ancestor_Tags (T : Tag) return Tag_Array is
      TSD_Ptr : constant Addr_Ptr := To_Addr_Ptr (To_Address (T));
      TSD : constant Type_Specific_Data_Ptr :=
                      To_Type_Specific_Data_Ptr (TSD_Ptr.all);
      Iface_Table : constant Interface_Data_Ptr := TSD.Interfaces_Table;
   begin
      if Iface_Table = null then
         declare
            Table : Tag_Array (1 .. 0);
         begin
            return Table;
         end;
      else
         declare
            Table : Tag_Array (1 .. Iface_Table.Nb_Ifaces);
         begin
            for J in 1 .. Iface_Table.Nb_Ifaces loop
               Table (J) := Iface_Table.Ifaces_Table (J).Iface_Tag;
            end loop;
            return Table;
         end;
      end if;
   end Interface_Ancestor_Tags;

end Loop_Optimization2;
