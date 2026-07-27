// PR c++/126310
// { dg-do run { target c++11 } }
// { dg-additional-options -O }

typedef int ValuePart;
struct base0  {
  int t;
};
struct base1 {
  void gg(ValuePart&&) {symbols = 1; }
  int symbols;
};
struct sub : base0, base1 {};

using EncodeFnTy = void (sub::*)(int&&);
static const EncodeFnTy encode_fns[]
{
    &sub::gg,
    &sub::gg
};

[[gnu::noinline,gnu::noclone]]
void f(sub &m, bool i)
{
 (&m->*encode_fns[i])(0);
}
int main()
{
    sub a;
    a.t = 0;
    a.symbols = 0;
    f(a, 0);
    if (a.t != 0)
      __builtin_abort ();
    if (a.symbols != 1)
      __builtin_abort ();
}
