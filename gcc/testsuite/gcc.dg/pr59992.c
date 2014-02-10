/* PR debug/59992 */
/* { dg-do compile } */
/* { dg-require-effective-target run_expensive_tests } */
/* { dg-options "-O2 -g" } */
/* { dg-timeout-factor 16.0 } */

#define A(n) if (p[n]) { extern void foo##n (int, int, double, int, int); foo##n (p[n], 5, 8.0, p[n] + p[n + 1], 9); return; }
#define B(n) A(n##0) A(n##1) A(n##2) A(n##3) A(n##4) A(n##5) A(n##6) A(n##7) A(n##8) A(n##9)
#define C(n) B(n##0) B(n##1) B(n##2) B(n##3) B(n##4) B(n##5) B(n##6) B(n##7) B(n##8) B(n##9)
#define D(n) C(n##0) C(n##1) C(n##2) C(n##3) C(n##4) C(n##5) C(n##6) C(n##7) C(n##8) C(n##9)
#define E(n) D(n##0) D(n##1) D(n##2) D(n##3) D(n##4) D(n##5) D(n##6) D(n##7) D(n##8) D(n##9)

void
foo (int *p)
{
  E(1)
}
