generic

   type Value is private;
   Init_Val : Value;

package Opt25_Pkg2 is

   type Stack (Size : Natural) is private;

   function Default_Stack return Stack;

private
   type Value_Array is array (Natural range <>) of Value;

   type Stack (Size : Natural) is record
      Store : Value_Array (1 .. Size);
   end record;

   Default_Stack_Var : Stack (10);
end Opt25_Pkg2;
