/* PR middle-end/112881 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-O2 -std=c23" } */

struct S { _BitInt(64) b; };

struct S
foo (_BitInt(64) p)
{
  return (struct S) { p };
}

#if __BITINT_MAXWIDTH__ >= 3924
struct T { _BitInt(3924) b; };

struct T
bar (_BitInt(3924) p)
{
  return (struct T) { p };
}
#endif
