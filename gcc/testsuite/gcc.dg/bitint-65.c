/* PR c/113315 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-std=c23" } */

#if __BITINT_MAXWIDTH__ >= 535
_BitInt(535) x;
#else
_BitInt(64) x;
#endif
extern int a[];
extern char b[][10];

int
foo (void)
{
  return a[x];
}

int
bar (void)
{
  return __builtin_strlen (b[x]);
}
