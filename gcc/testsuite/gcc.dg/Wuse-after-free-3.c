/* PR middle-end/104232 - spurious -Wuse-after-free after conditional free
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

char* f (void);

static inline void freep (void *p)
{
  __builtin_free (*(void**)p);    // { dg-bogus "-Wuse-after-free" }
}

int test_no_warn (void)
{
  __attribute__ ((__cleanup__ (freep))) char *s = 0, *t = 0;

  t = f ();
  if (!t)
    return 0;

  s = f ();
  return 1;
}
