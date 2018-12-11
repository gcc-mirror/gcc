with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package BIP_CU_T is
   type T is limited private;
   function Make_T (Name : String) return T;
private
   type T is limited record
      Name : Unbounded_String;
   end record;
end BIP_CU_T;
