/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-vect-cost-model" } */

typedef unsigned short (FUNC_P) (void *, unsigned char *, unsigned short);

void crashIt(int id, FUNC_P *func, unsigned char *funcparm)
{
  unsigned char buff[5], reverse[4];
  unsigned char *bp = buff;
  unsigned char *rp = reverse;
  unsigned short int count = 0;
  unsigned short cnt;
  while (id > 0)
    {
      *rp++ = (unsigned char) (id & 0x7F);
      id >>= 7;
      count++;
    }
  cnt = count + 1;
  while ((count--) > 1)
    {
      *bp++ = (unsigned char)(*(--rp) | 0x80);
    }
  *bp++ = *(--rp);
  (void)(*func)(funcparm, buff, cnt);
}
