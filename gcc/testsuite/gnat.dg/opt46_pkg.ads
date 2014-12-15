generic
   type Table_Component_Type is private;
   type Table_Index_Type     is range <>;

   Table_Low_Bound : Table_Index_Type;

package Opt46_Pkg is

   type Table_Type is
     array (Table_Index_Type range <>) of Table_Component_Type;
   subtype Big_Table_Type is
     Table_Type (Table_Low_Bound .. Table_Index_Type'Last);

   type Table_Ptr is access all Big_Table_Type;

   type Table_Private is private;

   type Instance is record
      Table : aliased Table_Ptr := null;
      P : Table_Private;
   end record;

   function Last (T : Instance) return Table_Index_Type;

private

   type Table_Private is record
      Last_Val : Integer;
   end record;

end Opt46_Pkg;
