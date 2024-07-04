/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mavxvnniint8 -mavxvnniint16 -O2 -mavxvnni" } */
/* { dg-final { scan-assembler-times "vpdpbusd" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbssd" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbuud" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwssd" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwuud" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwusd" 1 } } */

int
__attribute__((noinline))
usdot_prodv8qi (unsigned char* p, char* q, int sum)
{
  for (int i = 0; i != 8; i++)
    sum += p[i] * q[i];
  return sum;
}

int
udot_prodv8qi (unsigned char* p, unsigned char* q, int sum)
{
  for (int i = 0; i != 8; i++)
    sum += p[i] * q[i];
  return sum;
}

int
sdot_prodv8qi (char* p, char* q, int sum)
{
  for (int i = 0; i != 8; i++)
    sum += p[i] * q[i];
  return sum;
}

int
usdot_prodv4hi (unsigned short* p, short* q, int sum)
{
  for (int i = 0; i != 4; i++)
    sum += p[i] * q[i];
  return sum;
}

int
sdot_prodv4hi (short* p, short* q, int sum)
{
  for (int i = 0; i != 4; i++)
    sum += p[i] * q[i];
  return sum;
}

int
udot_prodv4hi (unsigned short* p, unsigned short* q, int sum)
{
  for (int i = 0; i != 4; i++)
    sum += p[i] * q[i];
  return sum;
}
