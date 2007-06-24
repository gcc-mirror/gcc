/* { dg-do compile } */
/* { dg-options "-O3" } */

typedef struct
{
  unsigned exp[256];
}
expbap_t;

void
a52_bit_allocate (expbap_t * expbap)
{
  int i;
  unsigned *exp = expbap->exp;
  char *bap;

  while (i < 3 || exp[i] > exp[i - 1]);

  do {
    if (exp[i + 1] == exp[i])
      bap[i] = 0;
    i++;
  } while (i < 20);
}
