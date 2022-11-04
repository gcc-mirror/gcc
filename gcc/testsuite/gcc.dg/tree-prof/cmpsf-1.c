/* Testcase derived from gcc.c-torture/execute cmpsf-1.c
   which showed jump threading profile insanities.  */
/* { dg-options "-Ofast -fdump-tree-dom2-all" } */

#include <limits.h>

void abort();
extern void exit (int);

#define F 140
#define T 13

int
feq (float x, float y)
{
  if (x == y)
    return T;
  else
    return F;
}

int
fne (float x, float y)
{
  if (x != y)
    return T;
  else
    return F;
}

int
flt (float x, float y)
{
  if (x < y)
    return T;
  else
    return F;
}

int
fge (float x, float y)
{
  if (x >= y)
    return T;
  else
    return F;
}

int
fgt (float x, float y)
{
  if (x > y)
    return T;
  else
    return F;
}

int
fle (float x, float y)
{
  if (x <= y)
    return T;
  else
    return F;
}

float args[] =
{
  0.0F,
  1.0F,
  -1.0F, 
  __FLT_MAX__,
  __FLT_MIN__,
  0.0000000000001F,
  123456789.0F,
  -987654321.0F
};

int correct_results[] =
{
 T, F, F, T, F, T,                                             
 F, T, T, F, F, T,                                             
 F, T, F, T, T, F,                                             
 F, T, T, F, F, T,                                             
 F, T, T, F, F, T,                                             
 F, T, T, F, F, T,                                             
 F, T, T, F, F, T,                                             
 F, T, F, T, T, F,                                             
 F, T, F, T, T, F,                                             
 T, F, F, T, F, T,                                             
 F, T, F, T, T, F,                                             
 F, T, T, F, F, T,                                             
 F, T, F, T, T, F,                                             
 F, T, F, T, T, F,                                             
 F, T, T, F, F, T,                                             
 F, T, F, T, T, F,                                             
 F, T, T, F, F, T,                                             
 F, T, T, F, F, T,                                             
 T, F, F, T, F, T,                                             
 F, T, T, F, F, T,                                             
 F, T, T, F, F, T,                                             
 F, T, T, F, F, T,                                             
 F, T, T, F, F, T,                                             
 F, T, F, T, T, F,                                             
 F, T, F, T, T, F,                                             
 F, T, F, T, T, F,
 F, T, F, T, T, F,
 T, F, F, T, F, T,
 F, T, F, T, T, F,
 F, T, F, T, T, F,
 F, T, F, T, T, F,
 F, T, F, T, T, F,
 F, T, F, T, T, F,
 F, T, T, F, F, T,
 F, T, F, T, T, F,
 F, T, T, F, F, T,
 T, F, F, T, F, T,
 F, T, T, F, F, T,
 F, T, T, F, F, T,
 F, T, F, T, T, F,
 F, T, F, T, T, F,
 F, T, T, F, F, T,
 F, T, F, T, T, F,
 F, T, T, F, F, T,
 F, T, F, T, T, F,
 T, F, F, T, F, T,
 F, T, T, F, F, T,
 F, T, F, T, T, F,
 F, T, F, T, T, F,
 F, T, F, T, T, F,
 F, T, F, T, T, F,
 F, T, T, F, F, T,
 F, T, F, T, T, F,
 F, T, F, T, T, F,
 T, F, F, T, F, T,
 F, T, F, T, T, F,
 F, T, T, F, F, T,
 F, T, T, F, F, T,
 F, T, T, F, F, T,
 F, T, T, F, F, T,
 F, T, T, F, F, T,
 F, T, T, F, F, T,
 F, T, T, F, F, T,
 T, F, F, T, F, T,
};

void
test (void)
{
  int i, j, *res = correct_results;

  for (i = 0; i < 8; i++)
    {
      float arg0 = args[i];
      for (j = 0; j < 8; j++)
	{
	  float arg1 = args[j];

	  if (feq (arg0, arg1) != *res++)
	    abort ();
	  if (fne (arg0, arg1) != *res++)
	    abort ();
	  if (flt (arg0, arg1) != *res++)
	    abort ();
	  if (fge (arg0, arg1) != *res++)
	    abort ();
	  if (fgt (arg0, arg1) != *res++)
	    abort ();
	  if (fle (arg0, arg1) != *res++)
	    abort ();
	}
    }
}

int
main (void)
{
  int i;
  for (i=0; i<100; i++)
    test ();
  exit (0);
}

/* { dg-final-use-not-autofdo { scan-tree-dump-not "Invalid sum" "dom2" { xfail *-*-* } } } */
