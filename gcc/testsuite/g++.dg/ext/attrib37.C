// PR c++/43093
// { dg-do compile { target i?86-*-* } }

struct S {
  int x;
  S(const S &s) {}
};

S __attribute__((__stdcall__)) getS();

void test()
{
  S s = getS();
}
