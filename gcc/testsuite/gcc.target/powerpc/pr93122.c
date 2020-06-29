/* PR target/93122 */
/* { dg-require-effective-target power10_ok } */
/* { dg-do compile { target lp64 } } */
/* { dg-options "-fstack-clash-protection -mprefixed -mcpu=power10" } */

void bar (char *);

void
foo (void)
{
  char s[4294967296];
  bar (s);
}
