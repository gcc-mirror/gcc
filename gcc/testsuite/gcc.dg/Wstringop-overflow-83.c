/* PR  middle-end/103143 - ICE due to infinite recursion in pointer-query.cc
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

typedef __SIZE_TYPE__ size_t;

void foo (size_t x)
{
  struct T { char buf[64]; char buf2[64]; } t;
  char *p = &t.buf[8];
  char *r = t.buf2;
  size_t i;

  for (i = 0; i < x; i++)
    {
      r = __builtin_mempcpy (r, p, i);
      p = r + 1;
    }
}
