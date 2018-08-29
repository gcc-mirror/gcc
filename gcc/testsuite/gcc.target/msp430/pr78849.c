/* { dg-do compile } */
/* { dg-final { scan-assembler ".size.*instance.*52" } } */

struct t_inner
{
  __int20 a;
  char val1;
  __int20 b[3];
  char val2;
};

struct t_full
{
  __int20 array[2];
  char val1;
  struct t_inner bb[2];
  char val2;
};

struct t_full instance =
{
    {
      4231,
      3212,
    },
    5,
    {
        {
          87680,
	  20,
          {
            2534,
            3,
            41,
          },
	  55,
        },
        {
          567,
	  4,
          {
            43522,
            5433,
            454,
          },
	  88,
        },
    },
    8,
};
