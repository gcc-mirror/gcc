/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_shift } */

unsigned  long int __attribute__ ((aligned (64)))arr[100];
int i;

void negative_test_for_vectorshifts_via_mul_with_const ()
{
  for (i=0; i<=99; i++)
    arr[i] = arr[i] * 123;
}

void negative_test_for_vectorshifts_via_mul_with_negative_const ()
{
  for (i=0; i<=99; i++)
    arr[i] = arr[i] * (-123);
}

void negative_test_for_vectorshifts_via_mul_with_varable (int x)
{
  for (i=0; i<=99; i++)
    arr[i] = arr[i] * x;
}


/* { dg-final { scan-tree-dump-times "vectorized 0 loops" 3 "vect"  {target  { ! { vect_int_mult } } } } } */
/* { dg-final { scan-tree-dump-not "vect_recog_mult_pattern: detected" "vect" {target  { ! { vect_int_mult } } } } } */
