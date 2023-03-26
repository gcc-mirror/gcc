/* PR ipa/105685 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wsuggest-attribute=cold" } */

extern void foo (char *, char const *, int);

__attribute__((cold)) char *
bar (int x)
{
  static char b[42];
  foo (b, "foo", x);
  return b;
}

__attribute__((cold)) char *
baz (int x)		/* { dg-bogus "function might be candidate for attribute 'cold'" } */
{
  return bar (x);
}
