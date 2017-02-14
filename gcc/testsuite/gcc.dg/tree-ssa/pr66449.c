/* { dg-do compile } */
/* { dg-options "-O3" } */

#if __SIZEOF_POINTER__ != __SIZEOF_LONG__
#define long __INTPTR_TYPE__
#endif

void *fn1(void *p1, void *p2, long p3)
{
  long a = (long)p1, b = (long)p2, c = p3;

  while (c)
    {
      int d = ((int *)b)[0];

      c--;
      ((char *)a)[0] = d;
      a++;
    }
  return 0;
}

