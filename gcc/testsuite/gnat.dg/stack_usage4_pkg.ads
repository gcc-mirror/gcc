package Stack_Usage4_Pkg is

   subtype Name_Index_Type is Natural range 1 .. 63;

   type Bounded_String is record
      Len  : Name_Index_Type;
      Data : String (Name_Index_Type'Range);
   end record;

   function Get return Bounded_String;

end Stack_Usage4_Pkg;
