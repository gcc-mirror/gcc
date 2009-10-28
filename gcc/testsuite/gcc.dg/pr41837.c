/* PR middle-end/41837 */
/* { dg-do compile } */
/* { dg-options "-fipa-struct-reorg -O -fwhole-program -fprofile-generate" } */

typedef struct { int a, b; } T1;
typedef struct S1 *T2;
typedef struct S2 *T3;
typedef struct S3 *T4;
typedef struct S4 *T5;
struct S4 { union { int c; } d; };
struct S2 { int e; T2 f; int g; };
typedef struct { T3 h; } T6;
typedef struct { int i; } *T7;
struct S3 { T6 j; T7 k; };

void
f1 (T4 x)
{
  if (!x->j.h->e)
    f5 (x);
}

void
f2 (void)
{
  f6 (f1);
}

void
f3 (T5 x, T1 *y)
{
}

void
f4 (void)
{
  f7 (f3);
}
