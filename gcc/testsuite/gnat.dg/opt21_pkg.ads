with System;

package Opt21_Pkg is

   type R is record
      Ptr : System.Address := System.Null_Address;
   end record;

   type Obj is access all R;

   function Get_Object (Object : not null access R) return System.Address;

   function Convert (W : Obj) return System.Address;

end Opt21_Pkg;
