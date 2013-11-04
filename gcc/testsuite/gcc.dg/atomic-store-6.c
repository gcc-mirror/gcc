/* { dg-do run } */
/* { dg-require-effective-target sync_int_128_runtime } */
/* { dg-options "-mcx16" { target { i?86-*-* x86_64-*-* } } } */

__int128_t i;

int main()
{
  __atomic_store_16(&i, -1, 0);
  if (i != -1)
    __builtin_abort();
  return 0;
}
