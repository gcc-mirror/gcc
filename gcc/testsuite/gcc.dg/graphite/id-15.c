/* { dg-additional-options "-Wno-old-style-definition" } */
/* { dg-require-effective-target int32plus } */

typedef long unsigned int size_t;
extern void *memset (void *__s, int __c, size_t __n) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));

static void
encode (words, low, hi)
     long *words;
     unsigned long low;
     long hi;
{
  words[0] = ((low) & (((unsigned long) 1 << (sizeof(unsigned long) / 2)) - 1));
  words[1] = ((unsigned long) (low) >> sizeof(unsigned long) / 2);
  words[2] = ((hi) & (((unsigned long) 1 << (sizeof(unsigned long) / 2)) - 1));
  words[3] = ((unsigned long) (hi) >> sizeof(unsigned long) / 2);
}

static void
decode (words, low, hi)
     long *words;
     unsigned long *low;
     long *hi;
{
  *low = words[0] + words[1] * ((unsigned long) 1 << sizeof(unsigned long) / 2);
  *hi = words[2] + words[3] * ((unsigned long) 1 << sizeof(unsigned long) / 2);
}

int
neg_double (l1, h1, lv, hv)
     unsigned long l1;
     long h1;
     unsigned long *lv;
     long *hv;
{
  if (l1 == 0)
    {
      *lv = 0;
      *hv = - h1;
      return (*hv & h1) < 0;
    }
  else
    {
      *lv = -l1;
      *hv = ~h1;
      return 0;
    }
}

int
add_double (l1, h1, l2, h2, lv, hv)
     unsigned long l1, l2;
     long h1, h2;
     unsigned long *lv;
     long *hv;
{
  unsigned long l;
  long h;

  l = l1 + l2;
  h = h1 + h2 + (l < l1);

  *lv = l;
  *hv = h;
  return ((~((h1) ^ (h2)) & ((h1) ^ (h))) < 0);
}

int
mul_double (l1, h1, l2, h2, lv, hv)
     unsigned long l1, l2;
     long h1, h2;
     unsigned long *lv;
     long *hv;
{
  long arg1[4];
  long arg2[4];
  long prod[4 * 2];
  unsigned long carry;
  int i, j, k;
  unsigned long toplow, neglow;
  long tophigh, neghigh;

  encode (arg1, l1, h1);
  encode (arg2, l2, h2);

  memset ((char *) prod, 0, sizeof prod);

  for (i = 0; i < 4; i++)
    {
      carry = 0;
      for (j = 0; j < 4; j++)
	{
	  k = i + j;

	  carry += arg1[i] * arg2[j];

	  carry += prod[k];
	  prod[k] = ((carry) & (((unsigned long) 1 << (sizeof(unsigned long) / 2)) - 1));
	  carry = ((unsigned long) (carry) >> sizeof(unsigned long) / 2);
	}
      prod[i + 4] = carry;
    }

  decode (prod, lv, hv);



  decode (prod + 4, &toplow, &tophigh);
  if (h1 < 0)
    {
      neg_double (l2, h2, &neglow, &neghigh);
      add_double (neglow, neghigh, toplow, tophigh, &toplow, &tophigh);
    }
  if (h2 < 0)
    {
      neg_double (l1, h1, &neglow, &neghigh);
      add_double (neglow, neghigh, toplow, tophigh, &toplow, &tophigh);
    }
  return (*hv < 0 ? ~(toplow & tophigh) : toplow | tophigh) != 0;
}
