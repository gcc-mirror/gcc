/* Test to verify that -Wzero-length-bounds and not -Warray-bounds is
   issued for accesses to interior zero-length array members that are
   within the bounds of the enclosing struct.
   { dg-do compile }
   { dg-options "-O2 -Wall -fno-tree-vectorize" } */

void sink (void*);

struct A { int i; };
struct B { int j; struct A a[0]; };

struct C
{
  struct B b1;
  struct B b2;
};

char cbuf1[1 * sizeof (struct C)];
char cbuf2[2 * sizeof (struct C)] = { };

void test_C_global_buf (void)
{
  struct C *p = (struct C*)&cbuf1;

  p->b1.a[-1].i = 0;     // { dg-warning "\\\[-Warray-bounds" }
  p->b1.a[ 0].i = 0;     // { dg-warning "\\\[-Wzero-length-bounds" }
  p->b1.a[ 1].i = 0;     // { dg-warning "\\\[-Warray-bounds" }
  sink (p);

  p->b2.a[ 0].i = 0;     // { dg-warning "\\\[-Warray-bounds" }
  p->b2.a[ 1].i = 0;     // { dg-warning "\\\[-Warray-bounds" }
  sink (p);

  p = (struct C*)&cbuf2;
  p->b1.a[-1].i = 0;     // { dg-warning "\\\[-Warray-bounds" }
  p->b1.a[ 0].i = 0;     // { dg-warning "\\\[-Wzero-length-bounds" }
  p->b1.a[ 1].i = 0;     // { dg-warning "\\\[-Wzero-length-bounds" }
  sink (p);

  p->b2.a[ 0].i = 0;
  p->b2.a[ 1].i = 0;
  p->b2.a[ 2].i = 0;     // { dg-warning "\\\[-Warray-bounds" }
  p->b2.a[ 3].i = 0;     // { dg-warning "\\\[-Warray-bounds" }
  sink (p);
}
