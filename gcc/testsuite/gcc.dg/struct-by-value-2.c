/* This testcase caused a sanity check to abort on SPARC64
   because of a discrepancy between two functions involved
   in the calculation of structure layout.  */

/* { dg-do compile } */

struct S { float f1; int i1; int i2; float f2; };

extern void foo(struct S);

void bar(void)
{
  struct S s;
  foo(s);
}
