/* { dg-additional-options "-Wno-builtin-declaration-mismatch" } */

void *
memset (int, int, __SIZE_TYPE__);

void
mp (int xl)
{
  memset (xl, 0, sizeof xl);
}
