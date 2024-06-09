/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Og" } } */
/* { dg-options "-march=rv64gc_xtheadfmemidx" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_xtheadfmemidx" { target { rv32 } } } */

typedef union {
  double v;
  unsigned w;
} my_t;

double z;

double foo (int i, int j)
{

  if (j)
    {
      switch (i)
	{
	case 0:
	  return 1;
	case 1:
	  return 0;
	case 2:
	  return 3.0;
	}
    }

  if (i == 1)
    {
      my_t u;
      u.v = z;
      u.w = 1;
      z = u.v;
    }
  return z;
}

/* { dg-final { scan-assembler-not "th.lrd\t" } } */
