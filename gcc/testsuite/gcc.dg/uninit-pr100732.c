/* PR middle-end/100732 - ICE on sprintf %s with integer argument
   { dg-do compile }
   { dg-options "-O2 -Wall -fdump-tree-optimized" } */

void nowarn_s_i (char *d, int i)
{
  __builtin_sprintf (d, "%s", i);       // { dg-warning "\\\[-Wformat" }
}

void warn_s_i (char *d)
{
  int i;
  __builtin_sprintf (d, "%s", i);       // { dg-warning "\\\[-Wformat" }
                                        // { dg-warning "\\\[-Wuninitialized" "" { target *-*-* } .-1 }
}

void warn_i_i (char *d)
{
  int i;
  __builtin_sprintf (d, "%i", i);       // { dg-warning "\\\[-Wuninitialized" }
}
