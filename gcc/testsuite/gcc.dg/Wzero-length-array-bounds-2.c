/* Test to verify that -Wzero-length-bounds and not -Warray-bounds is
   issued for accesses to interior zero-length array members that are
   within the bounds of the enclosing struct.
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

/* pr102706: disabled warnings because the now-disabled conditions for the
   bogus warnings to come up do not take cost analysis into account, and often
   come up wrong.  */
/* { dg-additional-options "-Wno-stringop-overflow" } */

void sink (void*);

struct A { int i; };
struct B { int j; struct A a[0]; };

struct C
{
  struct B b1;
  struct B b2;
};


void test_B_ref (struct B *p)
{
  // References with negative indices are always diagnosed by -Warray-bounds
  // even though they could be considered the same as past the end accesses
  // to trailing zero-length arrays.
  p->a[-1].i = 0;       // { dg-warning "\\\[-Warray-bounds" }
  p->a[ 0].i = 0;
  p->a[ 1].i = 0;
  sink (p);

  p[1].a[-1].i = 0;     // { dg-warning "\\\[-Warray-bounds" }
  p[1].a[ 0].i = 0;
  p[1].a[ 1].i = 0;
}


void test_C_ref (struct C *p)
{
  p->b1.a[-1].i = 0;     // { dg-warning "\\\[-Warray-bounds" }
  p->b1.a[ 0].i = 0;     // { dg-warning "\\\[-Wzero-length-bounds" }
  p->b1.a[ 1].i = 0;     // { dg-warning "\\\[-Wzero-length-bounds" }

  // Accesses to trailing zero-length arrays are not diagnosed (should
  // they be?)
  p->b2.a[ 0].i = 0;
  p->b2.a[ 9].i = 0;
}


void test_C_decl (void)
{
  struct C c, *p = &c;

  p->b1.a[-1].i = 0;     // { dg-warning "\\\[-Warray-bounds" }

  // c.b1.a[0].i overlaps c.b2.j.
  p->b1.a[ 0].i = 0;     // { dg-warning "\\\[-Wzero-length-bounds" }

  // c.b1.a[1].i is past the end of c...
  p->b1.a[ 1].i = 0;     // { dg-warning "\\\[-Warray-bounds" }
  sink (p);

  // ...and so are references to all elements of c.b2.a
  p->b2.a[ 0].i = 0;     // { dg-warning "\\\[-Warray-bounds" }
  p->b2.a[ 1].i = 0;     // { dg-warning "\\\[-Warray-bounds" }
  sink (p);
}


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

  p->b2.a[ 0].i = 0;    // { dg-bogus "\\\[-Wstringop-overflow" "pr102706" }
                        //   { xfail { vect_slp_v2si_store_align &&  { ! vect_slp_v4si_store_unalign } } }
  p->b2.a[ 1].i = 0;
  p->b2.a[ 2].i = 0;     // { dg-warning "\\\[-Warray-bounds" }
  p->b2.a[ 3].i = 0;     // { dg-warning "\\\[-Warray-bounds" }
  sink (p);
}


void test_C_local_buf (void)
{
  char cbuf1[1 * sizeof (struct C)] = "";
  char cbuf2[2 * sizeof (struct C)] = { };

  struct C *p = (struct C*)&cbuf1;

  p->b1.a[-1].i = 1;     // { dg-warning "\\\[-Warray-bounds" }
  p->b1.a[ 0].i = 2;     // { dg-warning "\\\[-Wzero-length-bounds" }
  p->b1.a[ 1].i = 3;     // { dg-warning "\\\[-Warray-bounds" }
  sink (p);

  p->b2.a[ 0].i = 4;     // { dg-warning "\\\[-Warray-bounds" }
  p->b2.a[ 1].i = 5;     // { dg-warning "\\\[-Warray-bounds" }
  sink (p);

  p = (struct C*)&cbuf2;
  p->b1.a[-1].i = 6;     // { dg-warning "\\\[-Warray-bounds" }
  p->b1.a[ 0].i = 7;     // { dg-warning "\\\[-Wzero-length-bounds" }
  p->b1.a[ 1].i = 8;     // { dg-warning "\\\[-Wzero-length-bounds" }
  sink (p);

  p->b2.a[ 0].i = 9;    // { dg-bogus "\\\[-Wstringop-overflow" "pr102706" }
                        //   { xfail { vect_slp_v2si_store_align &&  { ! vect_slp_v4si_store_unalign } } }
  p->b2.a[ 1].i = 10;
  p->b2.a[ 2].i = 11;    // { dg-warning "\\\[-Warray-bounds" }
  p->b2.a[ 3].i = 12;    // { dg-warning "\\\[-Warray-bounds" }
  sink (p);
}
