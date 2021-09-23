/* PR middle-end/100732 - ICE on sprintf %s with integer argument
   { dg-do compile }
   { dg-options "-O2 -Wall -fdump-tree-optimized" } */

void gb (char *d, _Bool b)
{
  __builtin_sprintf (d, "%s", b);       // { dg-warning "\\\[-Wformat" }
}

void gi (char *d, int i)
{
  __builtin_sprintf (d, "%s", i);       // { dg-warning "\\\[-Wformat" }
}

void gd (char *d, double x)
{
  __builtin_sprintf (d, "%s", x);       // { dg-warning "\\\[-Wformat" }
}


struct X { int i; };

void gx (char *d, struct X x)
{
  __builtin_sprintf (d, "%s", x);       // { dg-warning "\\\[-Wformat" }
}

/* Also verify that the invalid sprintf call isn't folded to strcpy.
   { dg-final { scan-tree-dump-times "sprintf" 4 "optimized" } }
   { dg-final { scan-tree-dump-not "strcpy" "optimized" } } */
