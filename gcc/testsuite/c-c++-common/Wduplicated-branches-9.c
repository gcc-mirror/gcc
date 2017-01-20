/* PR c/64279 */
/* { dg-do compile } */
/* { dg-options "-Wduplicated-branches" } */

extern int *p, foo (void), a[10];
#define N 5
#define M 5
#define I i

void
f (int i)
{
  *p += i ? 1 : 1; /* { dg-warning "this condition has identical branches" } */
  *p += i ? N : M; /* { dg-bogus "this condition has identical branches" "" { xfail *-*-* } } */
  *p += i ? M : N; /* { dg-bogus "this condition has identical branches" "" { xfail *-*-* } } */
  *p += i ? i : i; /* { dg-warning "this condition has identical branches" } */
  *p += i ? i++ : i++; /* { dg-warning "this condition has identical branches" } */
  *p += i ? foo () : foo (); /* { dg-warning "this condition has identical branches" } */
  *p += i ? ({ i++; }) : ({ i++; }); /* { dg-warning "this condition has identical branches" } */
  *p += i ? a[i] : a[i]; /* { dg-warning "this condition has identical branches" } */
  *p += i ? a[5] : a[5]; /* { dg-warning "this condition has identical branches" } */
  *p += i ? a[N] : a[M]; /* { dg-bogus "this condition has identical branches" "" { xfail *-*-* } } */
  *p += i ? a[5] : a[M]; /* { dg-bogus "this condition has identical branches" "" { xfail *-*-* } } */
  *p += i ? a[M] : a[5]; /* { dg-bogus "this condition has identical branches" "" { xfail *-*-* } } */
  *p += i ? a[I] : a[I]; /* { dg-warning "this condition has identical branches" } */
  *p += i ? a[i] : a[I]; /* { dg-bogus "this condition has identical branches" "" { xfail *-*-* } } */
  *p += i ? a[I] : a[i]; /* { dg-bogus "this condition has identical branches" "" { xfail *-*-* } } */

  *p += i ?: 1;
  *p += i ?: M;
  *p += i ?: N;
  *p += i ?: i; /* { dg-warning "this condition has identical branches" "" { target c++ } } */
  *p += i ?: i++;
  *p += i ?: foo ();
  *p += i ?: ({ i++; });
  *p += i ?: a[i];
  *p += i ?: a[5];
  *p += i ?: a[M];
  *p += i ?: a[M];
  *p += i ?: a[5];
  *p += i ?: a[I];
  *p += i ?: a[I];
  *p += i ?: a[i];

  *p += (i > 5 ? (i > 10 ? i : i) : i); /* { dg-warning "this condition has identical branches" } */
}
