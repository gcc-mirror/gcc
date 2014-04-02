// PR c++/43093
// { dg-options "-pedantic" }
// { dg-do compile { target { { i?86-*-* x86_64-*-* } && ia32 } } }
// { dg-require-effective-target c++11 }

// c++11 attributes that apply to types are ignored for now

struct S {
  int x;
  S(const S &s) {}
};

S getS() [[gnu::__stdcall__]];  // { dg-warning "ignored" }

void test()
{
  S s = getS();
}
