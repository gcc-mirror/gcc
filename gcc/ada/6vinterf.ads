------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                           I N T E R F A C E S                            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.1 $
--                                                                          --
-- This specification is adapted from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

--  This is the OpenVMS version of this package which adds Float_Representation
--  pragmas to the IEEE floating point types to enusre they remain IEEE in
--  thse presence of a VAX_Float Float_Representatin configuration pragma.

--  It assumes integer sizes of 8, 16, 32 and 64 are available, and that IEEE
--  floating-point formats are available.

package Interfaces is
pragma Pure (Interfaces);

   type Integer_8  is range -2 **  7 .. 2 **  7 - 1;
   for Integer_8'Size use  8;

   type Integer_16 is range -2 ** 15 .. 2 ** 15 - 1;
   for Integer_16'Size use 16;

   type Integer_32 is range -2 ** 31 .. 2 ** 31 - 1;
   for Integer_32'Size use 32;

   type Integer_64 is range -2 ** 63 .. 2 ** 63 - 1;
   for Integer_64'Size use 64;

   type Unsigned_8  is mod 2 **  8;
   for Unsigned_8'Size use  8;

   type Unsigned_16 is mod 2 ** 16;
   for Unsigned_16'Size use 16;

   type Unsigned_32 is mod 2 ** 32;
   for Unsigned_32'Size use 32;

   type Unsigned_64 is mod 2 ** 64;
   for Unsigned_64'Size use 64;

   function Shift_Left
     (Value  : Unsigned_8;
      Amount : Natural)
     return    Unsigned_8;

   function Shift_Right
     (Value  : Unsigned_8;
      Amount : Natural)
      return   Unsigned_8;

   function Shift_Right_Arithmetic
     (Value  : Unsigned_8;
      Amount : Natural)
      return   Unsigned_8;

   function Rotate_Left
     (Value  : Unsigned_8;
      Amount : Natural)
      return   Unsigned_8;

   function Rotate_Right
     (Value  : Unsigned_8;
      Amount : Natural)
      return   Unsigned_8;

   function Shift_Left
     (Value  : Unsigned_16;
      Amount : Natural)
     return    Unsigned_16;

   function Shift_Right
     (Value  : Unsigned_16;
      Amount : Natural)
      return   Unsigned_16;

   function Shift_Right_Arithmetic
     (Value  : Unsigned_16;
      Amount : Natural)
      return   Unsigned_16;

   function Rotate_Left
     (Value  : Unsigned_16;
      Amount : Natural)
      return   Unsigned_16;

   function Rotate_Right
     (Value  : Unsigned_16;
      Amount : Natural)
      return   Unsigned_16;

   function Shift_Left
     (Value  : Unsigned_32;
      Amount : Natural)
     return    Unsigned_32;

   function Shift_Right
     (Value  : Unsigned_32;
      Amount : Natural)
      return   Unsigned_32;

   function Shift_Right_Arithmetic
     (Value  : Unsigned_32;
      Amount : Natural)
      return   Unsigned_32;

   function Rotate_Left
     (Value  : Unsigned_32;
      Amount : Natural)
      return   Unsigned_32;

   function Rotate_Right
     (Value  : Unsigned_32;
      Amount : Natural)
      return   Unsigned_32;

   function Shift_Left
     (Value  : Unsigned_64;
      Amount : Natural)
     return    Unsigned_64;

   function Shift_Right
     (Value  : Unsigned_64;
      Amount : Natural)
      return   Unsigned_64;

   function Shift_Right_Arithmetic
     (Value  : Unsigned_64;
      Amount : Natural)
      return   Unsigned_64;

   function Rotate_Left
     (Value  : Unsigned_64;
      Amount : Natural)
      return   Unsigned_64;

   function Rotate_Right
     (Value  : Unsigned_64;
      Amount : Natural)
      return   Unsigned_64;

   pragma Import (Intrinsic, Shift_Left);
   pragma Import (Intrinsic, Shift_Right);
   pragma Import (Intrinsic, Shift_Right_Arithmetic);
   pragma Import (Intrinsic, Rotate_Left);
   pragma Import (Intrinsic, Rotate_Right);

   --  Floating point types. We use the digits value to define the IEEE
   --  forms, otherwise a configuration pragma specifying VAX float can
   --  default the digits to an illegal value for IEEE.
   --  Note: it is harmless, and explicitly permitted, to include additional
   --  types in interfaces, so it is not wrong to have IEEE_Extended_Float
   --  defined even if the extended format is not available.

   type IEEE_Float_32       is digits 6;
   pragma Float_Representation (IEEE_Float, IEEE_Float_32);

   type IEEE_Float_64       is digits 15;
   pragma Float_Representation (IEEE_Float, IEEE_Float_64);

   type IEEE_Extended_Float is digits 15;
   pragma Float_Representation (IEEE_Float, IEEE_Extended_Float);

end Interfaces;
