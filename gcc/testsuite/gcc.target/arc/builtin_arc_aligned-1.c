/* { dg-do run } */
/* { dg-options "-O" } */

extern void abort (void);

/* In macros like optimized memset, we want to be able to decide what
   alignment a passed pointer has.  */
#define f(p) __builtin_arc_aligned (p, 4)

int main (void)
{
  int i;
  if (f (&i) == 0)
    abort ();
  return 0;
}
