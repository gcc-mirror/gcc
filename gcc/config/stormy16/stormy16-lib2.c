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
