template <int i>
struct S
{
  S () { union { int a; }; a = 0; }
};
S<0> s;
