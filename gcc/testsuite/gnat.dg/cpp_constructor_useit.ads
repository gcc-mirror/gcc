with Interfaces.C; use Interfaces.C;

package Cpp_Constructor_Useit is
   function My_Fn (Val : int) return int
   with Convention => Cpp;

   function My_Fn (Val : int) return int is (Val + 1);
end Cpp_Constructor_Useit;
