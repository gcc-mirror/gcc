/* { dg-do compile } */
/* { dg-options "-Ofast -fdump-tree-vrp1-details" } */

typedef unsigned short u16;
typedef unsigned char u8;
typedef unsigned int u32;
#define NNN 10

u32 f[NNN], t[NNN];

u16 Calc_crc8(u8 data, u16 crc )
{
  u8 i=0,x16=0,carry=0;
  for (i = 0; i < 8; i++)
    {
      x16 = (u8)((data & 1) ^ ((u8)crc & 1));
      data >>= 1;
 
      if (x16 == 1)
	{
	  crc ^= 0x4002;
	  carry = 1;
	}
      else
	carry = 0;
      crc >>= 1;
      if (carry)
	crc |= 0x8000;
      else
	crc &= 0x7fff;
    }
  return crc;
}
int main (int argc, char argv[])
{
  int i, j; u16 crc;
  for (j = 0; j < 10000000; j++)
    {
      for (i = 0; i < NNN; i++)
	{
	  f[i] = random(i);
	  t[i] = random(NNN - i - 1);
	}
      for (i=0; i<NNN; i++) 
	{
	  crc=Calc_crc8(f[i],(u16)argc);
	  crc=Calc_crc8(t[i],crc);
	}
    }
  return crc;
}

/* { dg-final { scan-tree-dump "Cancelling" "vrp1" } } */
/* { dg-final { cleanup-tree-dump "vrp1" } } */

