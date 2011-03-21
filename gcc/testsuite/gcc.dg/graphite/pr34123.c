/* { dg-do compile } */
/* { dg-options "-O2 -ftree-loop-linear" } */

/* Testcase by Martin Michlmayr <tbm@cyrius.com> */

static unsigned char sbox[256] = {
};
void MD2Transform (unsigned char state[16])
{
  unsigned char t = 0;
  int i, j;
  for (i = 0; i < 16; i++)
    {
      for (j = 0; j < 2; j++)
        t = (state[j] ^= sbox[t]);
      t += i;
    }
}
