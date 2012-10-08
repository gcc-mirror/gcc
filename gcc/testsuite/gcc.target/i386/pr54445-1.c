/* { dg-do run { target tls_runtime } } */
/* { dg-options "-O2" } */
/* { dg-add-options tls } */

__thread unsigned char tls_array[64];

unsigned char
__attribute__ ((noinline))
tls_array_lookup_with_negative_constant(long long int position) {
  return tls_array[position - 1];
}

int
main ()
{
  int i;

  for (i = 0; i < sizeof (tls_array) / sizeof (tls_array[0]); i++)
    tls_array[i] = i;

  for (i = 0; i < sizeof (tls_array) / sizeof (tls_array[0]); i++)
    if (i != tls_array_lookup_with_negative_constant (i + 1))
      __builtin_abort ();
  return 0;
}
