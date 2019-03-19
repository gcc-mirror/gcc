// PR target/89752
// { dg-do compile }

struct A { A (); ~A (); short c; };

void
foo ()
{
  A a0, a1;
  __asm volatile ("" : "+rm" (a0), "+rm" (a1));
}
