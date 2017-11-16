// PR c++/61566
// { dg-do compile { target c++11 } }

struct function
{
  template < typename _Functor>
  function (_Functor) {}
};

template <class U>
struct C
{
  template <typename T>
  void foo (T, function = [] {});
};

void bar ()
{
  C<int> c;
  c.foo (1);
}

// { dg-final { scan-assembler "_ZN8functionC1IZN1CIiE3fooIiEEvT_S_Ed_UlvE_EES4_" } }
// { dg-final { scan-assembler-not "_ZZN1CIiE3fooIiEEvT_8functionEd_NKUlvE_clEv" } }
