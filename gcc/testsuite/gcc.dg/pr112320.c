/* { dg-do compile } */
/* { dg-options "-O -g" } */

unsigned void0_effective_addr2;
int void0_i, void0_m, void0_p2;
void void0()
{
  void0_m = 800 - (void0_effective_addr2 & 5);
  int b1;
  void0_i = 0;
  for (; void0_i < void0_m; void0_i++)
    b1++;
  void0_p2 = b1++;
}
