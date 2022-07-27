/* { dg-do compile } */
/* { dg-options "-Ofast" } */
/* { dg-require-effective-target indirect_jumps } */

#include <setjmp.h>
struct longjmp_buf {
  jmp_buf buf;
};
void g ();
void f ()
{
  int i, n;
  long *a;
  long *args;
  struct longjmp_buf b;
  setjmp (b.buf);
  for (;;)
    {
      for (i = 0; i < n; i++)
        a[i] = args[i];
      g ();
    }
}
