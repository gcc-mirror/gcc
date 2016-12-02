/* { dg-do compile } */
/* { dg-options "-mcmse" } */

typedef struct
{
  unsigned char	  a :2;
  unsigned char	    :0;
  unsigned short  b :5;
  unsigned char	    :0;
  unsigned short  c :3;
  unsigned char	    :0;
  unsigned int	  d :9;
} test_st_1;

typedef struct
{
  unsigned short  a :7;
  unsigned char	    :0;
  unsigned char	  b :1;
  unsigned char	    :0;
  unsigned short  c :6;
} test_st_2;

typedef union
{
  test_st_1 st_1;
  test_st_2 st_2;
}test_un;

typedef union
{
  test_un un;
  struct
    {
      unsigned int v1;
      unsigned int v2;
      unsigned int v3;
      unsigned int v4;
    }values;
} read_un;


typedef void __attribute__ ((cmse_nonsecure_call)) (*foo_ns) (test_un);

int
main (void)
{
  read_un r;
  foo_ns f;

  f = (foo_ns) 0x200000;
  r.values.v1 = 0xFFFFFFFF;
  r.values.v2 = 0xFFFFFFFF;

  f (r.un);
  return 0;
}

/* { dg-final { scan-assembler "mov\tip, r4" } } */
/* { dg-final { scan-assembler "movw\tr4, #8063" } } */
/* { dg-final { scan-assembler "movt\tr4, 63" } } */
/* { dg-final { scan-assembler "ands\tr0, r4" } } */
/* { dg-final { scan-assembler "movw\tr4, #511" } } */
/* { dg-final { scan-assembler "ands\tr1, r4" } } */
/* { dg-final { scan-assembler "mov\tr4, ip" } } */
/* { dg-final { scan-assembler "lsrs\tr4, r4, #1" } } */
/* { dg-final { scan-assembler "lsls\tr4, r4, #1" } } */
/* { dg-final { scan-assembler "movs\tr2, r4" } } */
/* { dg-final { scan-assembler "movs\tr3, r4" } } */
/* { dg-final { scan-assembler "bl\t__gnu_cmse_nonsecure_call" } } */

