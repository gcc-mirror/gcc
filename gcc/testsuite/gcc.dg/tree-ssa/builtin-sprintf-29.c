/* PR middle-end/100732 - ICE on sprintf %s with integer argument
   { dg-do compile }
   { dg-options "-fpermissive -O2 -Wall -fdump-tree-optimized" } */

#define sprintf(d, f, ...) \
  __builtin___sprintf_chk (d, 0, 32, f, __VA_ARGS__)


void fi (int i, const char *s)
{
  sprintf (i, "%s", s);       // { dg-warning "\\\[-Wint-conversion" }
}

void gb (char *d, _Bool b)
{
  sprintf (d, "%s", b);       // { dg-warning "\\\[-Wformat" }
}

void gi (char *d, int i)
{
  sprintf (d, "%s", i);       // { dg-warning "\\\[-Wformat" }
}

void gd (char *d, double x)
{
  sprintf (d, "%s", x);       // { dg-warning "\\\[-Wformat" }
}


struct X { int i; };

void gx (char *d, struct X x)
{
  sprintf (d, "%s", x);       // { dg-warning "\\\[-Wformat" }
}


/* Also verify that the invalid sprintf call isn't folded to strcpy.
   { dg-final { scan-tree-dump-times "sprintf_chk" 5 "optimized" } }
   { dg-final { scan-tree-dump-not "strcpy" "optimized" } } */
