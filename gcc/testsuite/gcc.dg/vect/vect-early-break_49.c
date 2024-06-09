/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

void abort();
struct foostr {
  _Complex short f1;
  _Complex short f2;
};
struct foostr a[16] __attribute__ ((__aligned__(16))) = {};
struct foostr c[16] __attribute__ ((__aligned__(16)));
struct foostr res[16] = {};
void
foo (void)
{
  int i;
  for (i = 0; i < 16; i++)
    {
      if (c[i].f1 != res[i].f1)
 abort ();
      if (c[i].f2 != res[i].f2)
 abort ();
    }
}
