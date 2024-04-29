/* PR target/113950 */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mvsx" } */

/* Verify we do not ICE on the following.  */

void abort (void);

int main ()
{
  int i;
  vector signed long long vsll_result, vsll_expected_result;
  signed long long sll_arg1;

  sll_arg1 = 300;
  vsll_expected_result = (vector signed long long) {300, 300};
  vsll_result = __builtin_vsx_splat_2di (sll_arg1);  

  for (i = 0; i < 2; i++)
    if (vsll_result[i] != vsll_expected_result[i])
      abort();

  return 0;
}
