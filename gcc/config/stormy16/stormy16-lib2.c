typedef		 int HItype		__attribute__ ((mode (HI)));
typedef		 int SItype		__attribute__ ((mode (SI)));
typedef unsigned int USItype		__attribute__ ((mode (SI)));

typedef int word_type			__attribute__ ((mode (__word__)));

USItype
udivmodsi4(USItype num, USItype den, word_type modwanted)
{
  USItype bit = 1;
  USItype res = 0;

  while (den < num && bit && !(den & (1L<<31)))
    {
      den <<=1;
      bit <<=1;
    }
  while (bit)
    {
      if (num >= den)
	{
	  num -= den;
	  res |= bit;
	}
      bit >>=1;
      den >>=1;
    }
  if (modwanted) return num;
  return res;
}



SItype
__divsi3 (SItype a, SItype b)
{
  word_type neg = 0;
  SItype res;

  if (a < 0)
    {
      a = -a;
      neg = !neg;
    }

  if (b < 0)
    {
      b = -b;
      neg = !neg;
    }

  res = udivmodsi4 (a, b, 0);

  if (neg)
    res = -res;

  return res;
}



SItype
__modsi3 (SItype a, SItype b)
{
  word_type neg = 0;
  SItype res;

  if (a < 0)
    {
      a = -a;
      neg = 1;
    }

  if (b < 0)
    b = -b;

  res = udivmodsi4 (a, b, 1);

  if (neg)
    res = -res;

  return res;
}




SItype
__udivsi3 (SItype a, SItype b)
{
  return udivmodsi4 (a, b, 0);
}



SItype
__umodsi3 (SItype a, SItype b)
{
  return udivmodsi4 (a, b, 1);
}

SItype
__ashlsi3 (SItype a, SItype b)
{
  word_type i;
  
  if (b & 16)
    a <<= 16;
  if (b & 8)
    a <<= 8;
  for (i = (b & 0x7); i > 0; --i)
    a <<= 1;
  return a;
}

SItype
__ashrsi3 (SItype a, SItype b)
{
  word_type i;
  
  if (b & 16)
    a >>= 16;
  if (b & 8)
    a >>= 8;
  for (i = (b & 0x7); i > 0; --i)
    a >>= 1;
  return a;
}

USItype
__lshrsi3 (USItype a, USItype b)
{
  word_type i;
  
  if (b & 16)
    a >>= 16;
  if (b & 8)
    a >>= 8;
  for (i = (b & 0x7); i > 0; --i)
    a >>= 1;
  return a;
}

static const unsigned char __popcount_tab[] =
{
  0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4,1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,
  1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
  1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
  2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
  1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
  2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
  2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
  3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,4,5,5,6,5,6,6,7,5,6,6,7,6,7,7,8,
};

int
__popcounthi2 (unsigned int x)
{
  unsigned int ret;

  ret = __popcount_tab [x & 0xff];
  ret += __popcount_tab [(x >> 8) & 0xff];

  return ret;
}

int
__parityhi2 (unsigned int x)
{
  x ^= x >> 8;
  x ^= x >> 4;
  x &= 0xf;
  return (0x6996 >> x) & 1;
}

int
__ctzhi2 (unsigned int x)
{
  extern int __ctzsi2 (unsigned long);
  unsigned long y = x;

  return __ctzsi2 (y << 16) - 16;
}

int
__clzhi2 (unsigned int x)
{
  extern int __clzsi2 (unsigned long);

  return __clzsi2 (x) - 16;
}
