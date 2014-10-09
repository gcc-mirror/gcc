// { dg-do compile }
// { dg-options "-fsanitize=undefined -Wall -Wno-unused-variable -std=c++11" }

typedef const long int L;

__attribute__((no_sanitize_undefined)) void
foo (int *p, L *l)
{
  int &r = *p;
  auto &r2 = *p;
  L &lr = *l;
  auto &&r3 = *p;
}

struct U
{
  int a;
  void foo () {}
};

__attribute__((no_sanitize_undefined)) void
bar (U *p)
{
  p->foo ();
}

// { dg-final { scan-assembler-not "__ubsan_handle" } }
