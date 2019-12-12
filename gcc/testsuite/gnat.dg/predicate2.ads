package Predicate2 is

   type Optional_Name_Type is new String;

   subtype Name_Type is Optional_Name_Type
     with Dynamic_Predicate => Name_Type'Length > 0;
   --  A non case sensitive name

   subtype Value_Type is String;

   overriding function "=" (Left, Right : Optional_Name_Type) return Boolean;
   overriding function "<" (Left, Right : Optional_Name_Type) return Boolean;

end Predicate2;
