// Build don't link:

// Based on testcase by adriang@campbellsoft.com

struct Null {
  template <typename T> operator T*() { return 0; }
  template <typename C, typename T> operator T C::*() { return 0; }
#if WORK_AROUND
  typedef int pmf();
  template <typename C> operator pmf C::* () { return 0; }
#endif
} NULL;

int *pd = NULL;
int (*pf)() = NULL;
int Null::*pmd = NULL;
int (Null::*pmf)() = NULL; // gets bogus error - cannot convert - XFAIL *-*-*
