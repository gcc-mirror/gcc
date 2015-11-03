// Origin: PR 46527
// { dg-do compile }
// { dg-options "-gdwarf-2 -O0 -dA" }

template <typename T> struct Struct {
  double defined_later();
};
struct WrapperStruct {
  Struct<double> ClassInstantiated;
};
template <typename T> double
Struct<T>::defined_later()  // { dg-function-on-line {_ZN6StructIdE13defined_laterEv} { xfail { powerpc-ibm-aix* } } }
{
  return 1;
}
void InstantiateMethod() {
  Struct<double>().defined_later();
}
