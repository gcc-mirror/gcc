/* PR target/118560 */
/* { dg-do compile } */
/* { dg-options "-O1" } */

struct { _Decimal32 a; } b;
void foo (int, _Decimal32);

#define B(n) \
void				\
bar##n (int, _Decimal32 d)	\
{				\
  foo (n, 1);			\
  b.a = d;			\
}

#define C(n) B(n##0) B(n##1) B(n##2) B(n##3) B(n##4) B(n##5) B(n##6) B(n##7) B(n##8) B(n##9)
C(1) C(2) C(3) C(4) C(5)
