package body Opt21_Pkg is

   function Get_Object (Object : not null access R) return System.Address is
   begin
      return Object.Ptr;
   end;

   function Convert (W : Obj) return System.Address is
   begin
      if W = null then
         return System.Null_Address;
      else
         return Get_Object (W);
      end if;
   end;

end Opt21_Pkg;
