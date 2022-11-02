/* { dg-require-effective-target power10_ok } */
/* { dg-options "-O1 -mdejagnu-cpu=power10" } */

/* (high:DI (symbol_ref:DI ("var_48")..))) should not cause ICE. */
extern short var_48;
void
foo (double *r)
{
  if (var_48)
    *r = 1234.5678;
}

