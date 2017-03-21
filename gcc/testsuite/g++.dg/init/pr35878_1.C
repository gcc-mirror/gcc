// { dg-options "-O2 --std=gnu++11" }
// { dg-do compile }
// { dg-final { scan-assembler "test.*%rdi, %rdi" { target i?86-*-* x86_64-*-* } } }
#include <new>
#include <utility>

struct s1{
  int a;
  int b;
  int c;
};

void f1 (s1 * v, s1&& s)
{
	new (v) s1(std::move(s));
}

void f2 (s1 * v, s1&& s)
{
	*v = std::move(s);
}
