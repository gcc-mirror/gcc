/* { dg-options "--coverage -fpath-coverage" } */
/* { dg-do compile } */

#include <stdexcept>

/* This test is a collection of odd CFG shapes which has been shown to
   trigger ICEs.  */

/* A hard abort-like exit could lead to paths with only the exit node,
   which would then be an empty path after entry/exit prunng.

   BEGIN paths
   summary: 0/1  */
void xabort () { __builtin_exit (0); }
/* END */

/* Unconditional exceptions have the same effect as aborts
   w.r.t. empty paths.  */
int throws (int) { throw std::runtime_error("exception"); }

int _setjmp (void **);
/* Bad instrumentation would give
   'error: returns_twice call is not first in basic block 3'.

   BEGIN paths
   summary: 0/1  */
void set_jmp ()
/* END */
{
  _setjmp (0);
}

/* From g++.dg/torture/pr83482.C  */
void a();
struct b {
  virtual long c() { return 0L; }
  void m_fn2() { c(); }
} d;

/* BEGIN paths
   summary: 0/3  */
void e() {
/* END */
  d.m_fn2();
  try {
    a();
    _setjmp(0);
  } catch (...) {
  }
}

/* BEGIN paths
   summary: 0/1  */
void multiple_setjmp ()
/* END */
{
  _setjmp (0);
  _setjmp (0);
}

/* loop_setjmp and loop_setjmp_continue are based on patterns found in
   expat-2.5.0 tests/minicheck.c srunner_run_all.  */
void loop_setjmp (int n)
{
  for (int i = 0; i < n; ++i)
    _setjmp (0);
}

void loop_setjmp_continue (int n)
{
  for (int i = 0; i < n; ++i)
    {
      if (i < n / 4)
	if (_setjmp (0))
	  continue;

      if (_setjmp (0))
	continue;

      if (i < n / 2)
	if (_setjmp (0))
	  continue;
    }
}

void loop_setjmp_infinite (int n)
{
  for (;;)
    _setjmp (0);
}

/* BEGIN paths
   summary: 0/2  */
void multiple_conditional_setjmp (int a)
/* END */
{
  if (a)
    _setjmp (0);
  else
    _setjmp (0);

  _setjmp (0);
}

/* Infinite loops can create CFGs that are a bit different, e.g. no edge for
   skipping or brekaing out of the loop.  */
int id (int x) { return x; }
int infinite1 ()
{
  for (int c = 0; ; c++) id (c);
}

void infinite2 ()
{
  for (;;) {}
}

/* This function has multiple SCCs (loops), but one has too many internal paths
   (2^32).  If the scc-internal-paths is not limited this function will not
   compile in reasonable time.  */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcoverage-too-many-paths"
void too_many_paths_single_scc (int x)
{
  int z = 0;
  int c = 0;

  for (int i = 0; i != x; ++i)
    for (int k = i; k != x; ++k)
      c++;

  for (int i = 0; i != 100; ++i)
    {
      if (z + i + 0 < x) z++;
      if (z + i + 1 < x) z++;
      if (z + i + 2 < x) z++;
      if (z + i + 3 < x) z++;
      if (z + i + 4 < x) z++;
      if (z + i + 5 < x) z++;
      if (z + i + 6 < x) z++;
      if (z + i + 7 < x) z++;
      if (z + i + 8 < x) z++;
      if (z + i + 9 < x) z++;
      if (z + i + 10 < x) z++;
      if (z + i + 11 < x) z++;
      if (z + i + 12 < x) z++;
      if (z + i + 13 < x) z++;
      if (z + i + 14 < x) z++;
      if (z + i + 15 < x) z++;
      if (z + i + 16 < x) z++;
      if (z + i + 17 < x) z++;
      if (z + i + 18 < x) z++;
      if (z + i + 19 < x) z++;
      if (z + i + 20 < x) z++;
      if (z + i + 21 < x) z++;
      if (z + i + 22 < x) z++;
      if (z + i + 23 < x) z++;
      if (z + i + 24 < x) z++;
      if (z + i + 25 < x) z++;
      if (z + i + 26 < x) z++;
      if (z + i + 27 < x) z++;
      if (z + i + 28 < x) z++;
      if (z + i + 29 < x) z++;
      if (z + i + 30 < x) z++;
      if (z + i + 31 < x) z++;
    }
}
#pragma GCC diagnostic pop

/* { dg-final { run-gcov prime-paths { --prime-paths gcov-22.C } } } */
