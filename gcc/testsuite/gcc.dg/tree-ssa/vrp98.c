/* { dg-do compile } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-Os -fdisable-tree-evrp -fdump-tree-vrp1-details" } */

#include <stdint.h>
#include <limits.h>

typedef unsigned int word __attribute__((mode(word)));
typedef unsigned __int128 bigger_than_word;

int
foo (bigger_than_word a, word b, uint8_t c)
{
  /* Must fold use of t1 into use of b, as b is no wider than word_mode. */
  const uint8_t t1 = b % UCHAR_MAX;

  /* Must NOT fold use of t2 into use of a, as a is wider than word_mode. */
  const uint8_t t2 = a % UCHAR_MAX;

  /* Must fold use of t3 into use of c, as c is narrower than t3. */
  const uint32_t t3 = (const uint32_t)(c >> 1);

  uint16_t ret = 0;

  if (t1 == 1)
    ret = 20;
  else if (t2 == 2)
    ret = 30;
  else if (t3 == 3)
    ret = 40;
  /* Th extra condition below is necessary to prevent a prior pass from
     folding away the cast. Ignored in scan-tree-dump. */
  else if (t3 == 4)
    ret = 50;

  return ret;
}

/* { dg-final { scan-tree-dump "Folded into: if \\(_\[0-9\]+ == 1\\)" "vrp1" } } */
/* { dg-final { scan-tree-dump-not "Folded into: if \\(_\[0-9\]+ == 2\\)" "vrp1" } } */
/* { dg-final { scan-tree-dump "Folded into: if \\(_\[0-9\]+ == 3\\)" "vrp1" } } */
