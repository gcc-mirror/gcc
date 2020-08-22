/* Check that we produce sign- and zero-extended additions and
   subtractions, and that no (eliminable) test- or compare-instructions
   are used. */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not {\tcmp|\ttest|\tsub\.|\tadd\.|\tmovs} } } */
/* { dg-final { scan-assembler "\tadds" } } */
/* { dg-final { scan-assembler "\tsubs" } } */

#ifndef t
#define t signed char
#define s _sc
#endif

#ifndef t2
#define t2 int
#endif

#ifndef f
#define f0(a, s) a ## s
#define f(a, s) f0(a, s)
#endif

extern void g(int);

t2 f(a, s) (t2 a, t *b, int *c)
{
  t2 d = a + *b;
  *c = d == 0;
  return d;
}
t2 f(b, s) (t2 a, t *b, int *c)
{
  t2 d = a - *b;
  *c = d == 0;
  return d;
}
