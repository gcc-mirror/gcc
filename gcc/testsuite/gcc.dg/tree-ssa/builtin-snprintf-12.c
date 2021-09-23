/* PR middle-end/100732 - ICE on sprintf %s with integer argument
   { dg-do compile }
   { dg-options "-O2 -Wall -fdump-tree-optimized" } */

#define snprintf(d, n, f, ...)					\
  __builtin___snprintf_chk (d, n, 0, 32, f, __VA_ARGS__)

int n;

void gb (char *d, _Bool b)
{
  snprintf (d, n, "%s", b);  // { dg-warning "\\\[-Wformat" }
}

void gi (char *d, int i)
{
  snprintf (d, n, "%s", i);  // { dg-warning "\\\[-Wformat" }
}

void gd (char *d, double x)
{
  snprintf (d, n, "%s", x);  // { dg-warning "\\\[-Wformat" }
}


struct X { int i; };

void gx (char *d, struct X x)
{
  snprintf (d, n, "%s", x);  // { dg-warning "\\\[-Wformat" }
}


/* Also verify that the invalid sprintf call isn't folded to strcpy.
   { dg-final { scan-tree-dump-times "snprintf_chk" 4 "optimized" } }
   { dg-final { scan-tree-dump-not "strcpy" "optimized" } } */
