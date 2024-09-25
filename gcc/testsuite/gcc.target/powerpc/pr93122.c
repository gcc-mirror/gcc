/* PR target/93122 */
/* { dg-do compile { target lp64 } } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-fstack-clash-protection -mprefixed -mdejagnu-cpu=power10" } */

void bar (char *);

void
foo (void)
{
  char s[4294967296];
  bar (s);
}
