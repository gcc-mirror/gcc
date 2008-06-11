-- { dg-do compile }
-- { dg-options "-O -gnatws" }

package body Varsize_Copy is

   type Key_Mapping_Type is record
      Page : Page_Type;
      B    : Boolean;
   end record;

   type Key_Mapping_Array is array (Key_Type) of Key_Mapping_Type;

   type Set is record
      Key_Mappings : Key_Mapping_Array;
   end record;

   S : Set;

   function F (Key : Key_Type) return Page_Type is
   begin
      return S.Key_Mappings (Key).Page;
   end;

end Varsize_Copy;
