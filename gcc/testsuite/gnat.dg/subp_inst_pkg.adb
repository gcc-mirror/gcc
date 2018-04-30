with Ada.Unchecked_Conversion;
with System.Address_Image;
package body Subp_Inst_Pkg is

   function Image (Val : T_Access) return String is
      function Convert is new Ada.Unchecked_Conversion
         (T_Access, System.Address);
   begin
      return System.Address_Image (Convert (Val));
   end Image;

   function T_Image (Val : access T) return String is
      type T_Access is access all T;
      function Convert is new Ada.Unchecked_Conversion
         (T_Access, System.Address);
   begin
      return System.Address_Image (Convert (Val));
   end T_Image;

end Subp_Inst_Pkg;
