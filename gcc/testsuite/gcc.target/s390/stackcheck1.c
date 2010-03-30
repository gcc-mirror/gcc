/* The automatically chosen stack guard value caused an ICE in that
   case.  */

/* { dg-do compile } */
/* { dg-options "-O2 -mstack-size=4096" } */

extern void bar (char *);

void
foo ()
{
  char a[2500];
  bar (a);
}
