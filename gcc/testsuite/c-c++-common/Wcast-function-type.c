/* { dg-do compile } */
/* { dg-options "-Wcast-function-type" } */

int f(long);

typedef int (f1)(long);
typedef int (f2)(void*);
#ifdef __cplusplus
typedef int (f3)(...);
typedef void (f4)(...);
#else
typedef int (f3)();
typedef void (f4)();
#endif
typedef void (f5)(void);

f1 *a;
f2 *b;
f3 *c;
f4 *d;
f5 *e;

void
foo (void)
{
  a = (f1 *) f; /* { dg-bogus   "incompatible function types" } */
  b = (f2 *) f; /* { dg-warning "7:cast between incompatible function types" } */
  c = (f3 *) f; /* { dg-bogus   "incompatible function types" } */
  d = (f4 *) f; /* { dg-warning "7:cast between incompatible function types" } */
  e = (f5 *) f; /* { dg-bogus   "incompatible function types" } */
}
