// { dg-do compile }
// Origin: Giovanni Bajo <giovannibajo at gcc dot gnu dot org>
// DR159: Namespace qualification in declarators 

namespace N {
  namespace M {
    void f();
    void g();
  }
  void M::f(){}
  void N::M::g(){}
}
