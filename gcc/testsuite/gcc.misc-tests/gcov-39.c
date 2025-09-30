/* { dg-options "--coverage -fpath-coverage" } */
/* { dg-do run } */

/* An obvious use case of #pragma GCC suppress_coverage is to support
   contracts/pre- and post-conditions, without having the contradictions
   messing up the coverage reports.

   The tests are written in terms of prime path coverage, because it more
   accurately detects when things are *not* included.  Here's how gcov -b would
   report the branches:

	   #:   13:  REQUIRE(x >= 0);	// branch(100)
   branch  0 taken 100%
					// branch(end)

   The branch test would not detect if gcov had also printed the
   should-be-suppressed-branch:

   branch  0 taken 100%
   branch  1 taken 0%
*/

int identity (int x) { return x; }

#define REQUIRE(pred) do { \
	_Pragma ("GCC suppress_coverage begin") \
	if (!(pred)) return -1; \
	_Pragma ("GCC suppress_coverage end") \
    } while (0)
#define ENSURE(pred) do { \
	_Pragma ("GCC suppress_coverage begin") \
	if (!(pred)) return -1; \
	_Pragma ("GCC suppress_coverage end") \
    } while (0)

/* BEGIN paths
   summary: 1/1
   expect covered: 43(suppress) 48(suppress) 50

   There are really 5 prime paths through this function, but 4 of them should
   be suppressed.  */
int
contracts1 (int x, int y)
/* END */
{
  REQUIRE (x >= 0);
  REQUIRE (y >= 0);
  int z = x + y;
  ENSURE (z >= x && z >= y);
  return z;
}

/* BEGIN paths
   summary: 0/1
   expect: 60(suppress) 65(suppress) 67

   We're failing a precondition, which should not contribute to coverage
*/
int
contracts2 (int x, int y)
/* END */
{
  REQUIRE (x >= 0);
  REQUIRE (y >= 0);
  int z = x + y;
  ENSURE (z >= x && z >= y);
  return z;
}

/* BEGIN paths
   summary: 11/14

   This is the reference function.  It's body should be identical to
   disable_in_loopN.  All functions should be called with the same
   arguments, but disable different parts of the function.
   [[gnu::suppress_coverage]] may change the graph (insert blocks), so the
   number of paths may change slightly.  */
int
suppressed_in_loop (int len)
/* END */
{
  int x = len;
  x = identity (x);
  x *= 5;
  for (int i = 0; i < len; ++i)
    {
      x += identity (i);

      if (i > 5)
	x += 1;
    }

  return x;
}

/* BEGIN paths
   summary: 9/12

   We're definitely expecting not taking any path from the top into the THEN of
   (i > 5).
   expect: 108 110 110(suppress) 116(true) 117 110
*/
int
suppressed_in_loop1 (int len)
/* END */
{
  int x = len;
  x = identity (x);
  x *= 5;
  for (int i = 0; i < len; ++i)
    {
#pragma GCC suppress_coverage begin
      x += identity (i);
#pragma GCC suppress_coverage end

      if (i > 5)
	x += 1;
    }

  return x;
}

/* BEGIN paths
   summary: 6/7 */
int
suppressed_in_loop2 (int len)
/* END */
{
  int x = len;
  x = identity (x);
  x *= 5;
  for (int i = 0; i < len; ++i)
    {
      x += identity (i);

#pragma GCC suppress_coverage begin
      if (i > 5)
	x += 1;
#pragma GCC suppress_coverage end
    }

  return x;
}

/* BEGIN paths
   summary: 0/1

   By disabling the full loop we should only have a single path through the
   function, as-if the loop isn't there.  */
int
suppressed_in_loop3 (int len)
/* END */
{
  int x = len;
  x = identity (x);
  x *= 5;
#pragma GCC suppress_coverage begin
  for (int i = 0; i < len; ++i)
    {
      x += identity (i);

      if (i > 5)
	x += 1;
    }
#pragma GCC suppress_coverage end

  return x;
}

/* BEGIN paths
   summary: 1/8
   args: (0, 0, 0, 0)

   Killing the first if should remove a single path only.  */
void
pathcov004c (int a, int b, int c, int d)
/* END */
{
#pragma GCC suppress_coverage begin
  if (a)
    {
    }
  else
    /* We cannot syntactically put the pragma before the else, but since the
       control flow is associated with the then-statement and not the else
       keyword, putting the end after else works fine.  */
#pragma GCC suppress_coverage end
    {
      while (b-- > 0 && c-- > 0)
	{
	  if (d)
	    break;
	}
    }
}

/* BEGIN paths
   args: (0, 1, 0, 0)
   summary: 0/5

   We keep five paths:
   if (a) -> while -> if (d) -> exit
   if (a) -> exit

   if (d) -> while -> if (d)
   if (d) -> while -> exit
   while -> if (d) -> while

   We really only disable the loop condition and not the implicit jumps.  */

void
pathcov004d (int a, int b, int c, int d)
/* END */
{
  if (a)
    {}
  else
    {
#pragma GCC suppress_coverage begin
      while (b-- > 0 && c-- > 0)
#pragma GCC suppress_coverage end
	{
	  if (d)
	    break;
	}
    }
}

int
main ()
{
  contracts1 (2, 4);
  contracts2 (-2, 4);
  contracts2 (2, -4);
  suppressed_in_loop (10);
  suppressed_in_loop1 (10);
  suppressed_in_loop2 (10);
  suppressed_in_loop3 (10);
  pathcov004c (0, 0, 0, 0);
  pathcov004d (0, 1, 0, 0);
}

/* { dg-final { run-gcov prime-paths { --prime-paths-lines=both gcov-39.c } } } */
