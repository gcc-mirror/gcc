/* Test for format extensions beyond the C standard and X/Open standard.
   Test for printf formats.
*/
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -Wformat" } */

#include "format.h"

void
foo (quad_t q, u_quad_t uq, quad_t *qn, size_t z, size_t *zn, long long int ll,
     unsigned long long int ull, int i, unsigned int u, double d,
     char *s, void *p, wchar_t *ls, wint_t lc, int *n, long int l)
{
  /* As an extension, GCC allows the BSD length "q" for integer formats.
     This is largely obsoleted in C99 by %j, %ll and PRId64.
  */
  printf ("%qd%qi%qo%qu%qx%qX%qn", q, q, uq, uq, uq, uq, qn);
  printf ("%qf", d); /* { dg-warning "length" "bad use of %q" } */
  printf ("%qF", d); /* { dg-warning "length" "bad use of %q" } */
  printf ("%qe", d); /* { dg-warning "length" "bad use of %q" } */
  printf ("%qE", d); /* { dg-warning "length" "bad use of %q" } */
  printf ("%qg", d); /* { dg-warning "length" "bad use of %q" } */
  printf ("%qG", d); /* { dg-warning "length" "bad use of %q" } */
  printf ("%qa", d); /* { dg-warning "length" "bad use of %q" } */
  printf ("%qA", d); /* { dg-warning "length" "bad use of %q" } */
  printf ("%qc", i); /* { dg-warning "length" "bad use of %q" } */
  printf ("%qs", s); /* { dg-warning "length" "bad use of %q" } */
  printf ("%qp", p); /* { dg-warning "length" "bad use of %q" } */
  printf ("%qC", lc); /* { dg-warning "length" "bad use of %q" } */
  printf ("%qS", ls); /* { dg-warning "length" "bad use of %q" } */
  /* With a bad length GCC wants some argument, any argument,
     to devour with the format conversion, as a synchronisation heuristic.
     This may get improved later.
  */
  printf ("%qm", i); /* { dg-warning "length" "bad use of %q" } */
  /* As an extension, GCC allows the length "Z" as a synonym for "z".
     This was an extension predating C99 which should now be considered
     deprecated; use the standard "z" instead.
  */
  printf ("%Zd%Zi%Zo%Zu%Zx%ZX", z, z, z, z, z, z);
  printf ("%Zn", zn);
  printf ("%Zf", d); /* { dg-warning "length" "bad use of %Z" } */
  printf ("%ZF", d); /* { dg-warning "length" "bad use of %Z" } */
  printf ("%Ze", d); /* { dg-warning "length" "bad use of %Z" } */
  printf ("%ZE", d); /* { dg-warning "length" "bad use of %Z" } */
  printf ("%Zg", d); /* { dg-warning "length" "bad use of %Z" } */
  printf ("%ZG", d); /* { dg-warning "length" "bad use of %Z" } */
  printf ("%Za", d); /* { dg-warning "length" "bad use of %Z" } */
  printf ("%ZA", d); /* { dg-warning "length" "bad use of %Z" } */
  printf ("%Zc", i); /* { dg-warning "length" "bad use of %Z" } */
  printf ("%Zs", s); /* { dg-warning "length" "bad use of %Z" } */
  printf ("%Zp", p); /* { dg-warning "length" "bad use of %Z" } */
  printf ("%ZC", lc); /* { dg-warning "length" "bad use of %Z" } */
  printf ("%ZS", ls); /* { dg-warning "length" "bad use of %Z" } */
  printf ("%Zm", i); /* { dg-warning "length" "bad use of %Z" } */
  /* As an extension, GCC allows the length "L" on integer formats
     (but not %n) as a synonym for "ll".
     This should be considered deprecated.
  */
  printf ("%Ld%Li%Lo%Lu%Lx%LX", ll, ll, ull, ull, ull, ull);
  /* As an extension, derived from syslog, GCC allows the conversion
     specifier "m" for formatting strerror(errno).  This may be used
     with width, precision and the "-" flag, the same as %s.
  */
  printf ("%m%3m%.4m%5.6m");
  printf ("%*m", i);
  printf ("%.*m", i);
  printf ("%*.*m", i, i);
  printf ("%3.*m", i);
  printf ("%*.4m", i);
  printf ("%-m");
  printf ("%+m"); /* { dg-warning "flag" "bad %+m" } */
  printf ("% m"); /* { dg-warning "flag" "bad % m" } */
  printf ("%#m"); /* { dg-warning "flag" "bad %#m" } */
  printf ("%0m"); /* { dg-warning "flag" "bad %0m" } */
  printf ("%'m"); /* { dg-warning "flag" "bad %'m" } */
  printf ("%hm", i); /* { dg-warning "length" "bad %hm" } */
  printf ("%hhm", i); /* { dg-warning "length" "bad %hhm" } */
  printf ("%lm", i); /* { dg-warning "length" "bad %lm" } */
  printf ("%llm", i); /* { dg-warning "length" "bad %llm" } */
  printf ("%jm", i); /* { dg-warning "length" "bad %jm" } */
  printf ("%zm", i); /* { dg-warning "length" "bad %zm" } */
  printf ("%tm", i); /* { dg-warning "length" "bad %tm" } */
  printf ("%Lm", i); /* { dg-warning "length" "bad %Lm" } */
  printf ("%qm", i); /* { dg-warning "length" "bad %qm" } */
  printf ("%Zm", i); /* { dg-warning "length" "bad %Zm" } */
  /* It should be OK to mix %m formats with $ operand number formats.  */
  printf ("%2$ld%m%1$d", i, l);
  /* Likewise, %m formats with width and precision should not have an
     operand number for the %m itself.
  */
  printf ("%*2$.*1$m", i, i);
  printf ("%1$*2$.*1$m", i, i); /* { dg-warning "no argument" "printf %1\$m" } */
  /* As an extension, glibc includes the "I" flag for decimal integer
     formats, to output using the locale's digits (e.g. in Arabic).
     In GCC, we require this to be in the standard place for flags, though
     glibc allows it also after width or precision.
  */
  printf ("%Id%Ii%Iu", i, i, u);
  printf ("%Io", u); /* { dg-warning "flag" "bad use of I flag" } */
  printf ("%Ix", u); /* { dg-warning "flag" "bad use of I flag" } */
  printf ("%IX", u); /* { dg-warning "flag" "bad use of I flag" } */
  printf ("%In", n); /* { dg-warning "flag" "bad use of I flag" } */
  printf ("%If", d); /* { dg-warning "flag" "bad use of I flag" } */
  printf ("%IF", d); /* { dg-warning "flag" "bad use of I flag" } */
  printf ("%Ie", d); /* { dg-warning "flag" "bad use of I flag" } */
  printf ("%IE", d); /* { dg-warning "flag" "bad use of I flag" } */
  printf ("%Ig", d); /* { dg-warning "flag" "bad use of I flag" } */
  printf ("%IG", d); /* { dg-warning "flag" "bad use of I flag" } */
  printf ("%Ia", d); /* { dg-warning "flag" "bad use of I flag" } */
  printf ("%IA", d); /* { dg-warning "flag" "bad use of I flag" } */
  printf ("%Ic", i); /* { dg-warning "flag" "bad use of I flag" } */
  printf ("%Is", s); /* { dg-warning "flag" "bad use of I flag" } */
  printf ("%Ip", p); /* { dg-warning "flag" "bad use of I flag" } */
  printf ("%IC", lc); /* { dg-warning "flag" "bad use of I flag" } */
  printf ("%IS", ls); /* { dg-warning "flag" "bad use of I flag" } */
  printf ("%Im"); /* { dg-warning "flag" "bad use of I flag" } */

  /* As an extension, GCC does format checking on "unlocked"
     i.e. thread unsafe versions of these functions.  */
  fprintf_unlocked (stdout, "%d", i);
  fprintf_unlocked (stdout, "%ld", i); /* { dg-warning "format" "fprintf_unlocked" } */
  printf_unlocked ("%d", i);
  printf_unlocked ("%ld", i); /* { dg-warning "format" "printf_unlocked" } */
}
