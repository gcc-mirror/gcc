/* PR middle-end/100732 - ICE on sprintf %s with integer argument
   { dg-do compile }
   { dg-options "-O2 -Wall -fdump-tree-optimized" } */

char d[32];

void gb (_Bool b)
{
  __builtin_snprintf (d, 32, "%s", b);  // { dg-warning "\\\[-Wformat" }
}

void gi (int i)
{
  __builtin_snprintf (d, 32, "%s", i);  // { dg-warning "\\\[-Wformat" }
}

void gd (char *d, double x)
{
  __builtin_snprintf (d, 32, "%s", x);  // { dg-warning "\\\[-Wformat" }
}


struct X { int i; };

void gx (char *d, struct X x)
{
  __builtin_snprintf (d, 32, "%s", x);  // { dg-warning "\\\[-Wformat" }
}

/* Also verify that the invalid sprintf call isn't folded to strcpy.
   { dg-final { scan-tree-dump-times "snprintf" 4 "optimized" } }
   { dg-final { scan-tree-dump-not "strcpy" "optimized" } } */
