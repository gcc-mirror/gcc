/* { dg-do run } */
/* { dg-options "-O2 -fno-tree-ccp -fno-tree-dominator-opts" } */

/* Tests generated via  */

#if 0
#include <stdio.h>
int main(int argc, char **argv)
{
  int amin, amax, bmin, bmax, a, b;
  int testno = 0;
  int min = atoi (argv[1]);
  int max = atoi (argv[2]);
  char op = argv[3][0];
  printf ("/* Testing range [%d, %d] with operator %c.  */\n", min, max, op);
  printf ("extern void abort (void);\n");
  for (amin = min; amin <= max; ++amin)
    for (amax = amin; amax <= max; ++amax)
      for (bmin = min; bmin <= max; ++bmin)
	for (bmax = bmin; bmax <= max; ++bmax)
	  {
	    ++testno;
	    printf ("int test%d (int a, int b)\n"
		    "{\n"
		    "  if (a >= %d && a <= %d && b >= %d && b <= %d)\n"
		    "   {\n"
		    "      int x = a %c b;\n"
		    "      if (0\n", testno, amin, amax, bmin, bmax, op);
	    for (a = amin; a <= amax; ++a)
	      for (b = bmin; b <= bmax; ++b)
		printf ("|| x == (%d %c %d)\n", a, op, b);
	    printf ("         ) return 0;\n"
		    "      abort ();\n"
		    "   }\n"
		    "  return 0;\n"
		    "}\n");
	  }
  printf ("int main()\n"
	  "{\n"
	  "  int a, b;\n"
	  "  for (a = %d; a <= %d; ++a)\n"
	  "    for (b = %d; b <= %d; ++b)\n"
	  "      {\n", min, max, min, max);
  for (; testno > 0; --testno)
    printf ("      test%d (a, b);\n", testno);
  printf ("      }\n"
	  "  return 0;\n"
	  "}\n");
  return 0;
}
#endif

extern void abort (void);

int test381 (int a, int b)
{
  if (a >= -3 && a <= -1 && b >= -2 && b <= 3)
    {
      int x = a | b;
      if (x == (-3 | -2)
	  || x == (-3 | -1)
	  || x == (-3 | 0)
	  || x == (-3 | 1)
	  || x == (-3 | 2)
	  || x == (-3 | 3)
	  || x == (-2 | -2)
	  || x == (-2 | -1)
	  || x == (-2 | 0)
	  || x == (-2 | 1)
	  || x == (-2 | 2)
	  || x == (-2 | 3)
	  || x == (-1 | -2)
	  || x == (-1 | -1)
	  || x == (-1 | 0)
	  || x == (-1 | 1)
	  || x == (-1 | 2)
	  || x == (-1 | 3))
	return 0;
      abort ();
    }
  return 0;
}

int test900 (int a, int b)
{
  if (a >= -1 && a <= 2 && b >= 3 && b <= 3)
    {
      int x = a & b;
      if (x == (-1 & 3)
	  || x == (0 & 3)
	  || x == (1 & 3)
	  || x == (2 & 3))
	return 0;
      abort ();
    }
  return 0;
}

int main()
{
  int a, b;
  for (a = -4; a < 4; ++a)
    for (b = -4; b < 4; ++b)
      {
	test381 (a, b);
	test900 (a, b);
      }

  return 0;
}
