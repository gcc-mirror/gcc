package body Incomplete5_Pkg is

   function Get_Handle (Object: Base_Object) return Access_Type is
   begin
      return Object.Handle;
   end;

   function From_Handle (Handle: Access_Type) return Base_Object is
   begin
      return (Handle=>Handle);
   end;

end Incomplete5_Pkg;
