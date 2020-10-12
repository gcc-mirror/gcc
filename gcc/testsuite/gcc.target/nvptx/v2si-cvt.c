/* { dg-do assemble } */
/* { dg-options "-O2 -save-temps" } */

typedef int __v2si __attribute__((__vector_size__(8)));

__v2si __attribute__((unused))
vector_cvt (__v2si arg)
{
  unsigned short *p = (unsigned short*)&arg;

  volatile unsigned short s = p[0];

  return arg;
}

__v2si __attribute__((unused))
vector_cvt_2 (__v2si arg)
{
  unsigned char *p = (unsigned char*)&arg;

  volatile unsigned char s = p[0];

  return arg;
}

/* Todo: We'd like to generate insns with .x operands to access the v2si
   operands, but that's currently not done, see PR96403.  */
