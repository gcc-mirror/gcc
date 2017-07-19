/* { dg-do assemble } */
/* { dg-options "-O2 -save-temps" } */

typedef int __v2si __attribute__((__vector_size__(8)));

int __attribute__((unused))
vector_cvt (__v2si arg)
{
  __v2si val4 = arg;
  char *p = (char*)&val4;

  if (p[0] != 1)
    return 1;
  if (p[1] != 2)
    return 1;
  if (p[2] != 3)
    return 1;

  return 0;
}

int
vector_cvt_2 (__v2si val, __v2si val2)
{
  char *p = (char*)&val;
  char *p2 = (char*)&val2;

  if (p[0] != p2[0])
    return 1;
  if (p[4] != p2[4])
    return 1;

  return 0;
}

/* We want to test for 'mov.t' here, but given PR80845 we test for cvt.t.t
   instead.
   { dg-final { scan-assembler "(?n)cvt\\.u32\\.u32.*\\.x" } } */
/* { dg-final { scan-assembler "(?n)cvt\\.u16\\.u32.*\\.x" } } */
