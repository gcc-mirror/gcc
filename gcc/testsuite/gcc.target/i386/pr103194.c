/* { dg-do compile } */
/* { dg-options "-O2" } */
long pscc_a_2_3;
int pscc_a_1_4;
void pscc()
{
  pscc_a_1_4 = __sync_fetch_and_and(&pscc_a_2_3, 1);
}

static int si;
long
test_types (long n)
{
  unsigned int u2 = __atomic_fetch_xor (&si, 0, 5);
  return u2;
}
