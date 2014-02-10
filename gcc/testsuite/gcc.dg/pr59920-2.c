/* PR tree-optimization/59920 */
/* { dg-do compile } */
/* { dg-options "-O0" } */

void *bar (void **);
void *baz (int, void **);

#define A(n) \
  { __label__ l1_##n, l2_##n, l3_##n;			\
    static void *a[] = { &&l1_##n, &&l2_##n, &&l3_##n };\
    void *b = bar (a);					\
    goto *b;						\
   l1_##n:						\
    b = baz (1, a);					\
    goto *b;						\
   l2_##n:						\
    b = baz (2, a);					\
    goto *b;						\
   l3_##n:;						\
  }
#define B(n) A(n##0) A(n##1) A(n##2) A(n##3) A(n##4) \
	     A(n##5) A(n##6) A(n##7) A(n##8) A(n##9)
#define C(n) B(n##0) B(n##1) B(n##2) B(n##3) B(n##4) \
	     B(n##5) B(n##6) B(n##7) B(n##8) B(n##9)

void
foo (void)
{
  C(1)
}
