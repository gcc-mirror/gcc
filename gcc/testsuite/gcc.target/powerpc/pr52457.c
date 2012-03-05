/* { dg-do run { target { powerpc*-*-linux* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-skip-if "" { powerpc*-*-*spe* } { "*" } { "" } } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-O1 -mcpu=power7" } */

extern void abort (void);

typedef long long T;
typedef T vl_t __attribute__((vector_size(2 * sizeof (T))));

vl_t
buggy_func (T x)
{
  vl_t w;
  T *p = (T *)&w;
  p[0] = p[1] = x;
  return w;
}

int
main(void)
{
  vl_t rval;
  T *pl;

  pl = (T *) &rval;
  rval = buggy_func (2);

  if (pl[0] != 2 || pl[1] != 2)
    abort ();

  return 0;
}
