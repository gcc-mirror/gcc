/* { dg-do compile } */
/* { dg-options "-O2 -g -fno-dce -fno-tree-dce" } */

extern void *buf;

extern void bar (void);

int
foo (int i)
{
  int j = 0;
  if (__builtin_setjmp (buf) == 0)
    {
      while (1)
      {
        j = 1;
	  bar ();
	  }
    }
  return j ? i : 0;
}
