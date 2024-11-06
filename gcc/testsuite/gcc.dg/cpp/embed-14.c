/* { dg-do compile } */
/* { dg-options "-std=gnu23 -Wnonnull" } */

#define A(n) int *p##n
#define B(n) A(n##0), A(n##1), A(n##2), A(n##3), A(n##4), A(n##5), A(n##6), A(n##7)
#define C(n) B(n##0), B(n##1), B(n##2), B(n##3), B(n##4), B(n##5), B(n##6), B(n##7)
#define D C(0), C(1), C(2), C(3)

void foo (D) __attribute__((nonnull (	/* { dg-message "in a call to function 'foo' declared 'nonnull'" } */
#embed __FILE__ limit (128)
)));
[[gnu::nonnull (
#embed __FILE__ limit (128)
)]] void bar (D);	/* { dg-message "in a call to function 'bar' declared 'nonnull'" } */

#undef A
#define A(n) nullptr

void
baz ()
{
  foo (D);	/* { dg-warning "argument \[0-9]\+ null where non-null expected" } */
  bar (D);	/* { dg-warning "argument \[0-9]\+ null where non-null expected" } */
}
