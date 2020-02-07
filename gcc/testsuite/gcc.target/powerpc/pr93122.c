/* PR target/93122 */
/* { dg-do compile { target lp64 } } */
/* { dg-options "-fstack-clash-protection -mprefixed-addr -mfuture" } */

void bar (char *);

void
foo (void)
{
  char s[4294967296];
  bar (s);
}
