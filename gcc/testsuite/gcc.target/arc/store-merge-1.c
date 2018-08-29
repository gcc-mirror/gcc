/* { dg-do compile } */
/* { dg-options "-O3" } */

/* This tests checks if we use st w6,[reg] format.  */

typedef struct {
  unsigned long __val[2];
} sigset_t;

int sigemptyset2 (sigset_t *set)
{
  set->__val[0] = 0;
  set->__val[1] = 0;
  return 0;
}

/* { dg-final { scan-assembler-times "st 0,\\\[r" 2 } } */
