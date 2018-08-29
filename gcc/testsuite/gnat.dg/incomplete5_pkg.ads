generic
   type Record_Type;
package Incomplete5_Pkg is

   type Access_Type is access Record_Type;

   type Base_Object is tagged record
      Handle: Access_Type;
   end record;

   function Get_Handle(Object: Base_Object) return Access_Type;

   function From_Handle(Handle: Access_Type) return Base_Object;

end Incomplete5_Pkg;
