typedef struct
{
  unsigned char	  a :2;
  unsigned char	    :0;
  unsigned short  b :5;
  float		  f;
} test_st_1;

typedef union
{
  test_st_1 st_1;
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
fn (foo_ns fptr)
{
  read_un r;

  r.values.v1 = 0xFFFFFFFF;
  r.values.v2 = 0xFFFFFFFF;
  r.values.v3 = 0xFFFFFFFF;
  r.values.v4 = 0xFFFFFFFF;

  fptr (r.un);
  return 0;
}
