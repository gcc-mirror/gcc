/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_shift } */

unsigned  long int __attribute__ ((aligned (64)))arr[100];
int i;

void test_for_vectorshifts_via_mul_with_power2_const ()
{
  for (i=0; i<=99; i++)
    arr[i] = arr[i] * 4;
}

void test_for_vectorshifts_via_mul_with_negative_power2_const ()
{
  for (i=0; i<=99; i++)
    arr[i] = arr[i] * (-4);
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect"  {target  { ! { vect_int_mult } } } } } */
/* { dg-final { scan-tree-dump-times "vect_recog_mult_pattern: detected" 2 "vect" {target  { ! { vect_int_mult } } } } } */
