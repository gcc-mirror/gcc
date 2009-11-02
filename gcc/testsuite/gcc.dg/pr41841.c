/* PR tree-optimization/41841 */
/* { dg-do compile } */
/* { dg-options "-O -fipa-struct-reorg -fwhole-program -fipa-cp" } */

typedef struct S *T;
typedef struct { } *U;
extern int f1 (void);

static void
f3 (U x, int y)
{
  T a = (T) x;
  y && f1 ();
}

static void
f2 (T x)
{
  f3 ((U) x, 1);
}

void *volatile a __attribute__((used)) = f2;
