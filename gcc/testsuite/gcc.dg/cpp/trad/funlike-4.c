/* Test that undefined names evaluate to zero, that macros after a
   funlike macro are expanded, and that if it is a '(' the funlike
   macro is not treated as such.  */

/* { dg-do run } */

extern void abort (void);

#define f(x) x

int main ()
{
#if f(1) == f /**/ (/**/1/**/)
  int x;
#endif

  x = 0;
  if (f
      /**/   (
	      /**/ 0/**/
	      /**/)
      )
    abort ();

  return 0;
}
