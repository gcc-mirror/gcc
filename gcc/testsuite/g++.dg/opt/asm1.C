// PR c++/6747
// { dg-do compile }
// { dg-options "-O" }

void foo()
{
  union { double d; char c[sizeof(double)]; } tmp;
  __asm__ ("" : "=m" (tmp.d));	// { dg-bogus "not directly addressable" }
}
