/* { dg-do compile } */
/* { dg-options "-ffixed-cr2 -ffixed-cr3 -ffixed-cr4" } */

#define SET_CR(R,V) __asm__ __volatile__ ("mtcrf %0,%1" : : "n" (1<<(7-R)), "r" (V<<(4*(7-R))) : "cr" #R)

void foo (void)
{
  SET_CR (2, 7);
  SET_CR (3, 8);
  SET_CR (4, 9);
}
