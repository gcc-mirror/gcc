/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre" } */

unsigned f(unsigned x, unsigned y, _Bool b)
{
#define m (b?-1:0)
  return (x&m)|(y&~m);
#undef m
}

/* { dg-final { scan-tree-dump "# prephitmp_\[0-9\]+ = PHI <\[xy\]_\[0-9\]+\\(D\\)\[^,\]*, \[xy\]_\[0-9\]+\\(D\\)" "pre" } } */
