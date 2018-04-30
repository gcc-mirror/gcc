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

typedef union
{
  test_st_1 st_1;
  test_st_2 st_2;
  test_st_3 st_3;
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
  r.values.v3 = 0xFFFFFFFF;

  f (r.un);
  return 0;
}
