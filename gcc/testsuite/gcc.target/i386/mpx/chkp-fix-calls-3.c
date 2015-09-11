/* { dg-do compile } */
/* { dg-options "-O2 -fexceptions -fcheck-pointer-bounds -mmpx" } */

extern int f2 (const char*, int, ...);
extern long int f3 (int *);
extern void err (void) __attribute__((__error__("error")));

extern __inline __attribute__ ((__always_inline__)) int
f1 (int i, ...)
{
  if (__builtin_constant_p (i))
    {
      if (i)
	err ();
      return f2 ("", i);
    }

  return f2 ("", i);
}

int
test ()
{
  int i;

  if (f1 (0))
    if (f3 (&i))
      i = 0;

  return i;
}


