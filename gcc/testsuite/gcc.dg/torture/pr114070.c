/* { dg-do compile } */
/* { dg-additional-options "-fno-vect-cost-model" } */

int unresolved(unsigned dirmask, unsigned mask, int *unresolved_n)
{
  for (int i = 0; i < 1024; i++) {
    mask |= 1;
    if (!unresolved_n[i] || unresolved_n[i] & 70000)
      dirmask |= 1;
  }
  return (dirmask == mask);
}
