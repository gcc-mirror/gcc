// { dg-do assemble  }

typedef int Array_T[2];

struct S1 {
  S1(const Array_T&);
};

struct S2 {
  S1 g();
  Array_T a;
};

S1 S2::g()
{
  return S1(a);
}

void h()
{
  S2 s2;
  s2.g();
}
