/* Copyright (C) 2004 Free Software Foundation.

   Test for correctness of composite floating-point comparisons.

   Written by Paolo Bonzini, 26th May 2004.  */

extern void abort (void);

#define TEST(c) if ((c) != ok) abort ();
#define ORD(a, b) (((a) < (b)) || (a) >= (b))
#define UNORD(a, b) (!ORD ((a), (b)))
#define UNEQ(a, b) (!LTGT ((a), (b)))
#define UNLT(a, b) (((a) < (b)) || __builtin_isunordered ((a), (b)))
#define UNLE(a, b) (((a) <= (b)) || __builtin_isunordered ((a), (b)))
#define UNGT(a, b) (((a) > (b)) || __builtin_isunordered ((a), (b)))
#define UNGE(a, b) (((a) >= (b)) || __builtin_isunordered ((a), (b)))
#define LTGT(a, b) (((a) < (b)) || (a) > (b))

float pinf;
float ninf;
float NaN;

int iuneq (float x, float y, int ok)
{
  TEST (UNEQ (x, y));
  TEST (!LTGT (x, y));
  TEST (UNLE (x, y) && UNGE (x,y));
}

int ieq (float x, float y, int ok)
{
  TEST (ORD (x, y) && UNEQ (x, y));
}

int iltgt (float x, float y, int ok)
{
  TEST (!UNEQ (x, y));
  TEST (LTGT (x, y));
  TEST (ORD (x, y) && (UNLT (x, y) || UNGT (x,y)));
}

int ine (float x, float y, int ok)
{
  TEST (UNLT (x, y) || UNGT (x, y));
  TEST ((x < y) || (x > y) || UNORD (x, y));
}

int iunlt (float x, float y, int ok)
{
  TEST (UNLT (x, y));
  TEST (UNORD (x, y) || (x < y));
}

int ilt (float x, float y, int ok)
{
  TEST (ORD (x, y) && UNLT (x, y));
  TEST ((x <= y) && (x != y));
  TEST ((x <= y) && (y != x));
  TEST ((x != y) && (x <= y));
  TEST ((y != x) && (x <= y));
}

int iunle (float x, float y, int ok)
{
  TEST (UNLE (x, y));
  TEST (UNORD (x, y) || (x <= y));
}

int ile (float x, float y, int ok)
{
  TEST (ORD (x, y) && UNLE (x, y));
  TEST ((x < y) || (x == y));
  TEST ((y > x) || (x == y));
  TEST ((x == y) || (x < y));
  TEST ((y == x) || (x < y));
}

int iungt (float x, float y, int ok)
{
  TEST (UNGT (x, y));
  TEST (UNORD (x, y) || (x > y));
}

int igt (float x, float y, int ok)
{
  TEST (ORD (x, y) && UNGT (x, y));
  TEST ((x >= y) && (x != y));
  TEST ((x >= y) && (y != x));
  TEST ((x != y) && (x >= y));
  TEST ((y != x) && (x >= y));
}

int iunge (float x, float y, int ok)
{
  TEST (UNGE (x, y));
  TEST (UNORD (x, y) || (x >= y));
}

int ige (float x, float y, int ok)
{
  TEST (ORD (x, y) && UNGE (x, y));
  TEST ((x > y) || (x == y));
  TEST ((y < x) || (x == y));
  TEST ((x == y) || (x > y));
  TEST ((y == x) || (x > y));
}

int
main ()
{
  pinf = __builtin_inf ();
  ninf = -__builtin_inf ();
  NaN = __builtin_nan ("");

  iuneq (ninf, pinf, 0);
  iuneq (NaN, NaN, 1);
  iuneq (pinf, ninf, 0);
  iuneq (1, 4, 0);
  iuneq (3, 3, 1);
  iuneq (5, 2, 0);

  ieq (1, 4, 0);
  ieq (3, 3, 1);
  ieq (5, 2, 0);

  iltgt (ninf, pinf, 1);
  iltgt (NaN, NaN, 0);
  iltgt (pinf, ninf, 1);
  iltgt (1, 4, 1);
  iltgt (3, 3, 0);
  iltgt (5, 2, 1);

  ine (1, 4, 1);
  ine (3, 3, 0);
  ine (5, 2, 1);

  iunlt (NaN, ninf, 1);
  iunlt (pinf, NaN, 1);
  iunlt (pinf, ninf, 0);
  iunlt (pinf, pinf, 0);
  iunlt (ninf, ninf, 0);
  iunlt (1, 4, 1);
  iunlt (3, 3, 0);
  iunlt (5, 2, 0);

  ilt (1, 4, 1);
  ilt (3, 3, 0);
  ilt (5, 2, 0);

  iunle (NaN, ninf, 1);
  iunle (pinf, NaN, 1);
  iunle (pinf, ninf, 0);
  iunle (pinf, pinf, 1);
  iunle (ninf, ninf, 1);
  iunle (1, 4, 1);
  iunle (3, 3, 1);
  iunle (5, 2, 0);

  ile (1, 4, 1);
  ile (3, 3, 1);
  ile (5, 2, 0);

  iungt (NaN, ninf, 1);
  iungt (pinf, NaN, 1);
  iungt (pinf, ninf, 1);
  iungt (pinf, pinf, 0);
  iungt (ninf, ninf, 0);
  iungt (1, 4, 0);
  iungt (3, 3, 0);
  iungt (5, 2, 1);

  igt (1, 4, 0);
  igt (3, 3, 0);
  igt (5, 2, 1);

  iunge (NaN, ninf, 1);
  iunge (pinf, NaN, 1);
  iunge (ninf, pinf, 0);
  iunge (pinf, pinf, 1);
  iunge (ninf, ninf, 1);
  iunge (1, 4, 0);
  iunge (3, 3, 1);
  iunge (5, 2, 1);

  ige (1, 4, 0);
  ige (3, 3, 1);
  ige (5, 2, 1);

  return 0;
}
