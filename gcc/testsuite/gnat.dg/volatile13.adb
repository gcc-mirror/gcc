-- { dg-do compile }

package body Volatile13 is

   procedure Compute_Index_Map (Self : Shared_String) is
      Map : Index_Map_Access := Self.Index_Map;
   begin
      Map := new Index_Map (Self.Length);
   end;

end Volatile13;
