/* PR target/38736 */
/* { dg-skip-if "attribute ((aligned))" { ! { i?86-*-* x86_64-*-* } } } */
/* { dg-require-effective-target avx } */

/* Test compatibility of attribute ((aligned)) with and without -mavx.  */

extern int aligned_x (void);
extern int aligned_y_avx (void);
extern void abort (void);

int
main ()
{
  if (aligned_x () != aligned_y_avx ())
    abort ();

  return 0;
}
