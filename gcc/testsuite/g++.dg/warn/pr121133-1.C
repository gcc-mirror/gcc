// PR c++/121133
// { dg-do compile }
// { dg-options "-std=c++98 -Wno-long-long -pedantic-errors" }

__extension__ typedef long long L;
__extension__ long long a;
struct S {
  __extension__ long long b;
};

void
foo ()
{
  __extension__ long long c;
  c = c + (__extension__ (long long) 1);
}
