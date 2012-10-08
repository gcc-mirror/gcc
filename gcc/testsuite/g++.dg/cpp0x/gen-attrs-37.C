// PR c++/43093
// { dg-options "-std=c++11 -pedantic" }
// { dg-do compile { target { { i?86-*-* x86_64-*-* } && ia32 } } }

struct S {
  int x;
  S(const S &s) {}
};

S getS() [[gnu::__stdcall__]];

void test()
{
  S s = getS();
}
