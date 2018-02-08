typedef struct
{
  char a:3;
} test_st3;

typedef struct
{
  char a:3;
} test_st2;

typedef struct
{
  test_st2 st2;
  test_st3 st3;
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

  f (r.st);
  return 0;
}
