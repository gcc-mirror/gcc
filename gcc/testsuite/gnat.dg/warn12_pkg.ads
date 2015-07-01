with Interfaces.C; use Interfaces.C;
with System;

package Warn12_Pkg is

   Anysize_Array: constant := 0;

   type Sid_And_Attributes is record
      Sid        : System.Address;
      Attributes : Interfaces.C.Unsigned_Long;
   end record;

   type Sid_And_Attributes_Array
      is array (Integer range 0..Anysize_Array) of aliased Sid_And_Attributes;

   type Token_Groups is record
      GroupCount : Interfaces.C.Unsigned_Long;
      Groups     : Sid_And_Attributes_Array;
   end record;

end Warn12_Pkg;
