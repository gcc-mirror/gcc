--  { dg-do compile }

with Interfaces.C; use Interfaces.C;
with Cpp_Constructor_FP;
with Cpp_Constructor_Useit;

procedure Cpp_Constructor is
   F : Cpp_Constructor_FP.Class :=
     Cpp_Constructor_FP.Constructor (Cpp_Constructor_Useit.My_Fn'Access);
begin
   null;
end Cpp_Constructor;
