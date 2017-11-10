typedef struct
{
  unsigned char a;
  unsigned int	b : 3;
  unsigned int	c : 14;
  unsigned int	d : 1;
  struct {
      unsigned int    ee  : 2;
      unsigned short  ff  : 15;
  } e;
  unsigned char	g : 1;
  unsigned char	  : 4;
  unsigned char	h : 3;
} test_st;

typedef union
{
  test_st st;
  struct
    {
      unsigned int v1;
      unsigned int v2;
      unsigned int v3;
      unsigned int v4;
    }values;
} read_st;


typedef void __attribute__ ((cmse_nonsecure_call)) (*foo_ns) (test_st);

int
main (void)
{
  read_st r;
  foo_ns f;

  f = (foo_ns) 0x200000;
  r.values.v1 = 0xFFFFFFFF;
  r.values.v2 = 0xFFFFFFFF;
  r.values.v3 = 0xFFFFFFFF;
  r.values.v4 = 0xFFFFFFFF;

  f (r.st);
  return 0;
}
