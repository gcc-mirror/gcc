/* { dg-do compile } */
/* { dg-options "-O2" } */

extern int si;
long
test_types (void)
{
  unsigned int u2 = __atomic_fetch_xor (&si, 0, 5);
  return u2;
}
