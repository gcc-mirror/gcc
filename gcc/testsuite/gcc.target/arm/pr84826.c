/* { dg-do compile } */
/* { dg-options "-Ofast -fstack-clash-protection" } */

void d (void *);

void a ()
{
  int b;
  void bar (int c)
  {
    if (__builtin_expect (c, 0))
      ++b;
  }
  d (bar);
}
