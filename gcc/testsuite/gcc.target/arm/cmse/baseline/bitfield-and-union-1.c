/* { dg-do compile } */
/* { dg-options "-mcmse" } */

typedef struct
{
  unsigned short a :11;
} test_st_4;

typedef union
{
  char	      a;
  test_st_4 st4;
}test_un_2;

typedef struct
{
  unsigned char	  a;
  unsigned int	    :0;
  unsigned int	  b :1;
  unsigned short    :0;
  unsigned short  c;
  unsigned int	    :0;
  unsigned int	  d :21;
} test_st_3;

typedef struct
{
  unsigned char	  a :3;
  unsigned int	  b :13;
  test_un_2	  un2;
} test_st_2;

typedef union
{
  test_st_2 st2;
  test_st_3 st3;
}test_un_1;

typedef struct
{
  unsigned char	  a :2;
  unsigned char	    :0;
  unsigned short  b :5;
  unsigned char	    :0;
  unsigned char	  c :4;
  test_un_1	  un1;
} test_st_1;

typedef union
{
  test_st_1 st1;
  struct
    {
      unsigned int v1;
      unsigned int v2;
      unsigned int v3;
      unsigned int v4;
    }values;
} read_st_1;


typedef void __attribute__ ((cmse_nonsecure_call)) (*foo_ns) (test_st_1);

int
main (void)
{
  read_st_1 r;
  foo_ns f;

  f = (foo_ns) 0x200000;
  r.values.v1 = 0xFFFFFFFF;
  r.values.v2 = 0xFFFFFFFF;
  r.values.v3 = 0xFFFFFFFF;
  r.values.v4 = 0xFFFFFFFF;

  f (r.st1);
  return 0;
}

/* { dg-final { scan-assembler "mov\tip, r4" } } */
/* { dg-final { scan-assembler "movw\tr4, #7939" } } */
/* { dg-final { scan-assembler "movt\tr4, 15" } } */
/* { dg-final { scan-assembler "ands\tr0, r4" } } */
/* { dg-final { scan-assembler "movw\tr4, #65535" } } */
/* { dg-final { scan-assembler "movt\tr4, 2047" } } */
/* { dg-final { scan-assembler "ands\tr1, r4" } } */
/* { dg-final { scan-assembler "movs\tr4, #1" } } */
/* { dg-final { scan-assembler "movt\tr4, 65535" } } */
/* { dg-final { scan-assembler "ands\tr2, r4" } } */
/* { dg-final { scan-assembler "movw\tr4, #65535" } } */
/* { dg-final { scan-assembler "movt\tr4, 31" } } */
/* { dg-final { scan-assembler "ands\tr3, r4" } } */
/* { dg-final { scan-assembler "mov\tr4, ip" } } */
/* { dg-final { scan-assembler "lsrs\tr4, r4, #1" } } */
/* { dg-final { scan-assembler "lsls\tr4, r4, #1" } } */
/* { dg-final { scan-assembler "bl\t__gnu_cmse_nonsecure_call" } } */
