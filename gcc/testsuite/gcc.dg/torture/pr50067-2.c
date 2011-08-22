/* { dg-do run } */

/* Make sure data-dependence analysis does not compute a bogus
  distance vector for the different sized accesses.  */

extern int memcmp(const void *, const void *, __SIZE_TYPE__);
extern void abort (void);
short a[32] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31 };
short b[32] = { 4, 0, 5, 0, 6, 0, 7, 0, 8, 0, };
int main()
{
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  int i;
  if (sizeof (short) == 2)
    {
      for (i = 0; i < 32; ++i)
	{
	  a[i] = (*((char(*)[32])&a[0]))[i+8];
	}
      if (memcmp (&a, &b, sizeof (a)) != 0)
	abort ();
    }
#endif
  return 0;
}
