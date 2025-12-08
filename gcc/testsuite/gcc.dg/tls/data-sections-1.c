/* { dg-do run } */
/* { dg-require-effective-target tls_runtime } */
/* { dg-options "-fdata-sections" } */
/* { dg-add-options tls } */

__thread int i = 1;

int main (void)
{
  if (i != 1)
    __builtin_abort ();

  return 0;
}
