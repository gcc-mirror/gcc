/* Test for GCC diagnositc formats.  */
/* Origin: Kaveh Ghazi <ghazi@caip.rutgers.edu> */
/* { dg-do compile } */
/* { dg-options "-Wformat" } */

#include "format.h"

#define ATTRIBUTE_DIAG(F) __attribute__ ((__format__ (F, 1, 2))) __attribute__ ((__nonnull__));

/* Magic identifiers must be set before the attribute is used.  */

typedef long long __gcc_host_wide_int__;

typedef struct location_s
{
  const char *file;
  int line;
} location_t;

union tree_node;
typedef union tree_node *tree;

extern int diag (const char *, ...) ATTRIBUTE_DIAG(__gcc_diag__);
extern int tdiag (const char *, ...) ATTRIBUTE_DIAG(__gcc_tdiag__);
extern int cdiag (const char *, ...) ATTRIBUTE_DIAG(__gcc_cdiag__);
extern int cxxdiag (const char *, ...) ATTRIBUTE_DIAG(__gcc_cxxdiag__);

void
foo (int i, int i1, int i2, unsigned int u, double d, char *s, void *p,
     int *n, short int *hn, long int l, unsigned long int ul,
     long int *ln, long double ld, wint_t lc, wchar_t *ls, llong ll,
     ullong ull, unsigned int *un, const int *cn, signed char *ss,
     unsigned char *us, const signed char *css, unsigned int u1,
     unsigned int u2, location_t *loc, tree t1, union tree_node *t2,
     tree *t3, tree t4[])
{
  /* Acceptable C90 specifiers, flags and modifiers.  */
  diag ("%%");
  tdiag ("%%");
  cdiag ("%%");
  cxxdiag ("%%");
  diag ("%d%i%o%u%x%c%s%p%%", i, i, u, u, u, i, s, p);
  tdiag ("%d%i%o%u%x%c%s%p%%", i, i, u, u, u, i, s, p);
  cdiag ("%d%i%o%u%x%c%s%p%%", i, i, u, u, u, i, s, p);
  cxxdiag ("%d%i%o%u%x%c%s%p%%", i, i, u, u, u, i, s, p);
  diag ("%qd%qi%qo%qu%qx%qc%qs%qp%<%%%'%>", i, i, u, u, u, i, s, p);
  tdiag ("%qd%qi%qo%qu%qx%qc%qs%qp%<%%%'%>", i, i, u, u, u, i, s, p);
  cdiag ("%qd%qi%qo%qu%qx%qc%qs%qp%<%%%'%>", i, i, u, u, u, i, s, p);
  cxxdiag ("%qd%qi%qo%qu%qx%qc%qs%qp%<%%%'%>", i, i, u, u, u, i, s, p);
  diag ("%ld%li%lo%lu%lx", l, l, ul, ul, ul);
  tdiag ("%ld%li%lo%lu%lx", l, l, ul, ul, ul);
  cdiag ("%ld%li%lo%lu%lx", l, l, ul, ul, ul);
  cxxdiag ("%ld%li%lo%lu%lx", l, l, ul, ul, ul);
  diag ("%lld%lli%llo%llu%llx", ll, ll, ull, ull, ull);
  tdiag ("%lld%lli%llo%llu%llx", ll, ll, ull, ull, ull);
  cdiag ("%lld%lli%llo%llu%llx", ll, ll, ull, ull, ull);
  cxxdiag ("%lld%lli%llo%llu%llx", ll, ll, ull, ull, ull);
  diag ("%wd%wi%wo%wu%wx", ll, ll, ull, ull, ull);
  tdiag ("%wd%wi%wo%wu%wx", ll, ll, ull, ull, ull);
  cdiag ("%wd%wi%wo%wu%wx", ll, ll, ull, ull, ull);
  cxxdiag ("%wd%wi%wo%wu%wx", ll, ll, ull, ull, ull);
  diag ("%.*s", i, s);
  tdiag ("%.*s", i, s);
  cdiag ("%.*s", i, s);
  cxxdiag ("%.*s", i, s);

  /* Extensions provided in the diagnostic framework.  */
  diag ("%m");
  tdiag ("%m");
  cdiag ("%m");
  cxxdiag ("%m");

  tdiag ("%D%F%T%V", t1, t1, t1, t1);
  tdiag ("%+D%+F%+T%+V", t1, t1, t1, t1);
  tdiag ("%q+D%q+F%q+T%q+V", t1, t1, t1, t1);
  tdiag ("%D%D%D%D", t1, t2, *t3, t4[5]);
  cdiag ("%D%F%T%V", t1, t1, t1, t1);
  cdiag ("%+D%+F%+T%+V", t1, t1, t1, t1);
  cdiag ("%q+D%q+F%q+T%q+V", t1, t1, t1, t1);
  cdiag ("%D%D%D%D", t1, t2, *t3, t4[5]);
  cdiag ("%E", t1);
  cxxdiag ("%A%D%E%F%T%V", t1, t1, t1, t1, t1, t1);
  cxxdiag ("%D%D%D%D", t1, t2, *t3, t4[5]);
  cxxdiag ("%#A%#D%#E%#F%#T%#V", t1, t1, t1, t1, t1, t1);
  cxxdiag ("%+A%+D%+E%+F%+T%+V", t1, t1, t1, t1, t1, t1);
  cxxdiag ("%+#A%+#D%+#E%+#F%+#T%+#V", t1, t1, t1, t1, t1, t1);
  cxxdiag ("%C%L%O%P%Q", i, i, i, i, i);

  tdiag ("%v%qv%#v", i, i, i);
  cdiag ("%v%qv%#v", i, i, i);
  cxxdiag ("%v%qv%#v", i, i, i);

  /* Bad stuff with extensions.  */
  diag ("%m", i); /* { dg-warning "format" "extra arg" } */
  tdiag ("%m", i); /* { dg-warning "format" "extra arg" } */
  cdiag ("%m", i); /* { dg-warning "format" "extra arg" } */
  cxxdiag ("%m", i); /* { dg-warning "format" "extra arg" } */
  diag ("%#m"); /* { dg-warning "format" "bogus modifier" } */
  tdiag ("%#m"); /* { dg-warning "format" "bogus modifier" } */
  cdiag ("%#m"); /* { dg-warning "format" "bogus modifier" } */
  cxxdiag ("%#m"); /* { dg-warning "format" "bogus modifier" } */
  diag ("%+m"); /* { dg-warning "format" "bogus modifier" } */
  tdiag ("%+m"); /* { dg-warning "format" "bogus modifier" } */
  cdiag ("%+m"); /* { dg-warning "format" "bogus modifier" } */
  cxxdiag ("%+m"); /* { dg-warning "format" "bogus modifier" } */
  diag ("%D", t1); /* { dg-warning "format" "bogus tree" } */
  tdiag ("%A", t1); /* { dg-warning "format" "bogus tree" } */
  tdiag ("%E", t1);
  tdiag ("%#D", t1); /* { dg-warning "format" "bogus modifier" } */
  cdiag ("%A", t1); /* { dg-warning "format" "bogus tree" } */
  cdiag ("%#D", t1); /* { dg-warning "format" "bogus modifier" } */
  cdiag ("%+D", t1);
  cxxdiag ("%C"); /* { dg-warning "format" "missing arg" } */
  cxxdiag ("%C", l); /* { dg-warning "format" "wrong arg" } */
  cxxdiag ("%C", i, i); /* { dg-warning "format" "extra arg" } */
  cxxdiag ("%#C", i); /* { dg-warning "format" "bogus modifier" } */
  cxxdiag ("%+C", i); /* { dg-warning "format" "bogus modifier" } */
  tdiag ("%D"); /* { dg-warning "format" "missing arg" } */
  cdiag ("%D"); /* { dg-warning "format" "missing arg" } */
  cxxdiag ("%D"); /* { dg-warning "format" "missing arg" } */
  tdiag ("%D", i); /* { dg-warning "format" "wrong arg" } */
  cdiag ("%D", i); /* { dg-warning "format" "wrong arg" } */
  cxxdiag ("%D", i); /* { dg-warning "format" "wrong arg" } */
  tdiag ("%D", t1, t1); /* { dg-warning "format" "extra arg" } */
  cdiag ("%D", t1, t1); /* { dg-warning "format" "extra arg" } */
  cxxdiag ("%D", t1, t1); /* { dg-warning "format" "extra arg" } */

  tdiag ("%V", i); /* { dg-warning "format" "wrong arg" } */
  cdiag ("%V", i); /* { dg-warning "format" "wrong arg" } */
  cxxdiag ("%V", i); /* { dg-warning "format" "wrong arg" } */

  tdiag ("%v", t1); /* { dg-warning "format" "wrong arg" } */
  cdiag ("%v", t1); /* { dg-warning "format" "wrong arg" } */
  cxxdiag ("%v", t1); /* { dg-warning "format" "wrong arg" } */

  /* Standard specifiers not accepted in the diagnostic framework.  */
  diag ("%X\n", u); /* { dg-warning "format" "HEX" } */
  diag ("%f\n", d); /* { dg-warning "format" "float" } */
  diag ("%e\n", d); /* { dg-warning "format" "float" } */
  diag ("%E\n", d); /* { dg-warning "format" "float" } */
  diag ("%g\n", d); /* { dg-warning "format" "float" } */
  diag ("%G\n", d); /* { dg-warning "format" "float" } */
  diag ("%n\n", n); /* { dg-warning "format" "counter" } */
  diag ("%hd\n", i); /* { dg-warning "format" "conversion" } */

  /* Various tests of bad argument types.  */
  diag ("%-d", i); /* { dg-warning "format" "bad flag" } */
  tdiag ("%-d", i); /* { dg-warning "format" "bad flag" } */
  cdiag ("%-d", i); /* { dg-warning "format" "bad flag" } */
  cxxdiag ("%-d", i); /* { dg-warning "format" "bad flag" } */
  diag ("% d", i); /* { dg-warning "format" "bad flag" } */
  tdiag ("% d", i); /* { dg-warning "format" "bad flag" } */
  cdiag ("% d", i); /* { dg-warning "format" "bad flag" } */
  cxxdiag ("% d", i); /* { dg-warning "format" "bad flag" } */
  diag ("%#o", u); /* { dg-warning "format" "bad flag" } */
  tdiag ("%#o", u); /* { dg-warning "format" "bad flag" } */
  cdiag ("%#o", u); /* { dg-warning "format" "bad flag" } */
  cxxdiag ("%#o", u); /* { dg-warning "format" "bad flag" } */
  diag ("%0d", i); /* { dg-warning "format" "bad flag" } */
  tdiag ("%0d", i); /* { dg-warning "format" "bad flag" } */
  cdiag ("%0d", i); /* { dg-warning "format" "bad flag" } */
  cxxdiag ("%0d", i); /* { dg-warning "format" "bad flag" } */
  diag ("%08d", i); /* { dg-warning "format" "bad flag" } */
  tdiag ("%08d", i); /* { dg-warning "format" "bad flag" } */
  cdiag ("%08d", i); /* { dg-warning "format" "bad flag" } */
  cxxdiag ("%08d", i); /* { dg-warning "format" "bad flag" } */
  diag ("%+d\n", i); /* { dg-warning "format" "bad flag" } */
  tdiag ("%+d\n", i); /* { dg-warning "format" "bad flag" } */
  cdiag ("%+d\n", i); /* { dg-warning "format" "bad flag" } */
  cxxdiag ("%+d\n", i); /* { dg-warning "format" "bad flag" } */
  diag ("%3d\n", i); /* { dg-warning "format" "bad flag" } */
  tdiag ("%3d\n", i); /* { dg-warning "format" "bad flag" } */
  cdiag ("%3d\n", i); /* { dg-warning "format" "bad flag" } */
  cxxdiag ("%3d\n", i); /* { dg-warning "format" "bad flag" } */
  diag ("%-3d\n", i); /* { dg-warning "format" "bad flag" } */
  tdiag ("%-3d\n", i); /* { dg-warning "format" "bad flag" } */
  cdiag ("%-3d\n", i); /* { dg-warning "format" "bad flag" } */
  cxxdiag ("%-3d\n", i); /* { dg-warning "format" "bad flag" } */
  diag ("%.7d\n", i); /* { dg-warning "format" "bad flag" } */
  tdiag ("%.7d\n", i); /* { dg-warning "format" "bad flag" } */
  cdiag ("%.7d\n", i); /* { dg-warning "format" "bad flag" } */
  cxxdiag ("%.7d\n", i); /* { dg-warning "format" "bad flag" } */
  diag ("%+9.4d\n", i); /* { dg-warning "format" "bad flag" } */
  tdiag ("%+9.4d\n", i); /* { dg-warning "format" "bad flag" } */
  cdiag ("%+9.4d\n", i); /* { dg-warning "format" "bad flag" } */
  cxxdiag ("%+9.4d\n", i); /* { dg-warning "format" "bad flag" } */
  diag ("%.3ld\n", l); /* { dg-warning "format" "bad flag" } */
  tdiag ("%.3ld\n", l); /* { dg-warning "format" "bad flag" } */
  cdiag ("%.3ld\n", l); /* { dg-warning "format" "bad flag" } */
  cxxdiag ("%.3ld\n", l); /* { dg-warning "format" "bad flag" } */
  diag ("%d %lu\n", i, ul);
  diag ("%d", l); /* { dg-warning "format" "bad argument types" } */
  diag ("%wd", l); /* { dg-warning "format" "bad argument types" } */
  diag ("%d", ll); /* { dg-warning "format" "bad argument types" } */
  diag ("%*s", i, s); /* { dg-warning "format" "bad * argument types" } */
  diag ("%*.*s", i, i, s); /* { dg-warning "format" "bad * argument types" } */
  diag ("%*d\n", i1, i); /* { dg-warning "format" "bad * argument types" } */
  diag ("%.*d\n", i2, i); /* { dg-warning "format" "bad * argument types" } */
  diag ("%*.*ld\n", i1, i2, l); /* { dg-warning "format" "bad * argument types" } */
  diag ("%ld", i); /* { dg-warning "format" "bad argument types" } */
  diag ("%s", n); /* { dg-warning "format" "bad argument types" } */

  /* Wrong number of arguments.  */
  diag ("%d%d", i); /* { dg-warning "matching" "wrong number of args" } */
  diag ("%d", i, i); /* { dg-warning "arguments" "wrong number of args" } */
  /* Miscellaneous bogus constructions.  */
  diag (""); /* { dg-warning "zero-length" "warning for empty format" } */
  diag ("\0"); /* { dg-warning "embedded" "warning for embedded NUL" } */
  diag ("%d\0", i); /* { dg-warning "embedded" "warning for embedded NUL" } */
  diag ("%d\0%d", i, i); /* { dg-warning "embedded|too many" "warning for embedded NUL" } */
  diag (NULL); /* { dg-warning "null" "null format string warning" } */
  diag ("%"); /* { dg-warning "trailing" "trailing % warning" } */
  diag ((const char *)L"foo"); /* { dg-warning "wide" "wide string" } */
  diag ("%s", (char *)0); /* { dg-warning "null" "%s with NULL" } */

  /* Make sure we still get warnings for regular printf.  */
  printf ("%d\n", ll); /* { dg-warning "format" "bad argument types" } */
}
