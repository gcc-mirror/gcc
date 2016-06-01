/* { dg-do compile { target { ! { m68k*-*-* mmix*-*-* mep*-*-* bfin*-*-* v850*-*-* moxie*-*-* cris*-*-* m32c*-*-* fr30*-*-* mcore*-*-* powerpc*-*-* xtensa*-*-* hppa*-*-* nios2*-*-* } } } }  */
/* { dg-additional-options "-O2 -fdump-tree-vrp-details" }  */
/* { dg-additional-options "-mbranch-cost=2" { target mips*-*-* avr-*-* s390*-*-* i?86-*-* x86_64-*-* } }  */
/* { dg-final { scan-tree-dump-times "Threaded jump" 8 "vrp1" } }  */

void foo (void);
void bar (void);
void blah (void);

/* One jump threaded here.  */

void
baz_1 (int a, int b, int c)
{
  if (a && b)
    foo ();
  if (!b && c)
    bar ();
}

/* One jump threaded here.  */

void
baz_2 (int a, int b, int c)
{
  if (a && b)
    foo ();
  if (b || c)
    bar ();
}

/* One jump threaded here.  */

void
baz_3 (int a, int b, int c)
{
  if (a && b > 10)
    foo ();
  if (b < 5 && c)
    bar ();
}

/* Two jumps threaded here.  */

void
baz_4 (int a, int b, int c)
{
  if (a && b)
    {
      foo ();
      if (c)
        bar ();
    }
  if (b && c)
    blah ();
}

/* Two jumps threaded here.  */

void
baz_5 (int a, int b, int c)
{
  if (a && b)
    {
      foo ();
      if (c)
        bar ();
    }
  if (!b || !c)
    blah ();
}

/* One jump threaded here.  */

void
baz_6 (int a, int b, int c)
{
  if (a == 39 && b == 41)
    foo ();
  if (c == 12 || b == 41)
    bar ();
}
