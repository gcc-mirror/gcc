// PR c++/34914
// { dg-do compile }

struct A { int __attribute ((vector_size (8))) x; };

void
foo ()
{
  __attribute ((vector_size (8))) int A::*p;
  p == 0;
}
