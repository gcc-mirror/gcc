with Interfaces.C; use Interfaces.C;

package Cpp_Constructor_FP is
   type Class is limited record null; end record
   with Convention => Cpp, Import;

   function Constructor
     (Fn : access function (Val : int) return int) return Class;
   pragma Cpp_Constructor (Constructor, External_Name => "foo");
end Cpp_Constructor_FP;
