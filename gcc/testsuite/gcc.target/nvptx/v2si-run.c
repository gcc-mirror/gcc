/* { dg-do run } */
/* { dg-options "-O2" } */

#include "v2si.c"

void __attribute__((noinline, noclone))
init_val ( __v2si *p)
{
  char *p2 = (char*)p;
  p2[0] = 8;
  p2[1] = 7;
  p2[2] = 6;
  p2[3] = 5;
  p2[4] = 4;
  p2[5] = 3;
  p2[6] = 2;
  p2[7] = 1;
}

int
main (void)
{
  {
    __v2si val;
    __v2si val2;
    __v2si val3;

    init_val(&val);

    /* Copy val to val2.  */
    vector_store (&val2, val);

    /* Copy val2 to val3.  */
    val3 = vector_load (&val2);

    /* Compare val to val3.  */
    {
      char *p = (char*)&val;
      char *p2 = (char*)&val3;

      if (p[0] != p2[0])
	return 1;
      if (p[1] != p2[1])
	return 1;
      if (p[2] != p2[2])
	return 1;
      if (p[3] != p2[3])
	return 1;
      if (p[4] != p2[4])
	return 1;
      if (p[5] != p2[5])
	return 1;
      if (p[6] != p2[6])
	return 1;
      if (p[7] != p2[7])
	return 1;
    }
  }

  {
    __v2si val4 = vector_const ();
    char *p = (char*)&val4;

    if (p[0] != 1)
      return 1;
    if (p[1] != 0)
      return 1;
    if (p[2] != 0)
      return 1;
    if (p[3] != 0)
      return 1;
    if (p[4] != 2)
      return 1;
    if (p[5] != 0)
      return 1;
    if (p[6] != 0)
      return 1;
    if (p[7] != 0)
      return 1;
  }

  return 0;
}
