/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-cprop2" } */

int f (int);
unsigned int glob;

void
g ()
{
  while (f (glob));
  glob = 5;
}

/* { dg-final { scan-rtl-dump "GLOBAL COPY-PROP" "cprop2" } } */
