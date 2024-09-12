// { dg-do compile }
// { dg-skip-if "requires hosted libstdc++ for vector" { ! hostedlib } }
// This code used to be rejected as there was no conversion from int to float __complex__
 #include <vector>
 typedef float __complex__ fcomplex;
 std::vector<fcomplex> vfc(10);
