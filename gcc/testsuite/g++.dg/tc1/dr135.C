// { dg-do compile }
// Origin: Giovanni Bajo <giovannibajo at gcc dot gnu dot org>
// DR135: Class type in in-class member function definitions 

struct S {
  S f() { return S(); }  // { dg-bogus "" "incomplete class type is allowed as return type" }
  void g(S) { }          // { dg-bogus "" "incomplete class type is allowed as parameter type" }
};
