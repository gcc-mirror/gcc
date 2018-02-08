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
