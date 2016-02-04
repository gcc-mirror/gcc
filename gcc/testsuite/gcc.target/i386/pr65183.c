/* { dg-do compile { target { ! x32 } } } */
/* { dg-options "-O -fcheck-pointer-bounds -fchkp-use-nochk-string-functions -mmpx" } */

extern void bar(void *);
extern void baz(void);

static int lc[32];

void foobar(void *c)
{
  bar(&c);
  __builtin_memcpy (lc, c, lc[0]);
}

void foo ()
{
  baz ();
  foobar(0);
}
