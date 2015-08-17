// PR c++/67244
// { dg-do compile { target c++11 } }

class A {
public:
  int operator*();
};
template <typename T, typename Predicate>
void searchGen(int, int, T, Predicate p4) {
  p4(0);
}
template <typename...> struct B;
template <typename MetaFunction, typename Type, typename... Types>
struct B<MetaFunction, Type, Types...> {
  static void exec() { MetaFunction::template exec<Type>; }
};
template <typename MetaFunction, typename... Types> void forEachType() {
  B<MetaFunction, Types...>::exec;
}
namespace {
struct C {
  template <typename T> void exec() {
    A __trans_tmp_1;
    const auto target = *__trans_tmp_1;
    searchGen(0, 0, 0, [=](T) { [=] { target; }; });
  }
};
}
void ____C_A_T_C_H____T_E_S_T____75() { forEachType<C, int>; }
