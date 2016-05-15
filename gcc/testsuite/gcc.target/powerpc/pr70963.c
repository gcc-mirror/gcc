/* { dg-do run { target powerpc64*-*-* } } */
/* { dg-require-effective-target p8vector_hw } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-mcpu=power8" } */

#include <stdlib.h>
#include <stdio.h>
#include <altivec.h>

static int failed;
static void test (void);

static void check (int result, const char *name)
{
  if (!result)
    {
      failed++;
      printf ("fail %s\n", name);
    }
}
    
int main (void)
{
  test ();
  if (failed)
    abort ();
  return 0;
}

vector double x = { 81.0, 76.0 };
vector long long y = { 81, 76 };

static void test()
{
  vector long long a = vec_cts (x, 0);
  vector double b = vec_ctf (a, 0);
  vector long long c = __builtin_vsx_xvcvdpuxds_scale (x, 0);
  vector double d = vec_ctf (c, 0);
  check (vec_all_eq (a, y), "vec_cts");
  check (vec_all_eq (b, x), "vec_ctf");
  check (vec_all_eq (c, y), "xvcvdpuxds");
  check (vec_all_eq (d, x), "vec_ctf unsigned");
}
