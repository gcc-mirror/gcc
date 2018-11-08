/* { dg-do compile } */

struct S { int s; };
void foo (char *);
void bar (int, char *, struct S, int *);
#pragma omp declare target to (bar)
#define N 16

void
f1 (int sc1, struct S ag1, int *pt1)
{
  char ar1[N];
  foo (ar1);
  #pragma omp target defaultmap(default:scalar) defaultmap(to:aggregate) defaultmap(none:pointer)	/* { dg-error "enclosing 'target'" } */
  bar (sc1, ar1, ag1, pt1);	/* { dg-error "'pt1' not specified in enclosing 'target'" } */
}

void
f2 (int sc2, struct S ag2, int *pt2)
{
  char ar2[N];
  foo (ar2);
  #pragma omp target defaultmap(none:scalar) defaultmap(from:aggregate) defaultmap(default:pointer)	/* { dg-error "enclosing 'target'" } */
  bar (sc2, ar2, ag2, pt2);	/* { dg-error "'sc2' not specified in enclosing 'target'" } */
}

void
f3 (int sc3, struct S ag3, int *pt3)
{
  char ar3[N];
  foo (ar3);
  #pragma omp target defaultmap(firstprivate:scalar) defaultmap(none:aggregate) defaultmap(to:pointer)	/* { dg-error "enclosing 'target'" } */
  bar (sc3, ar3, ag3, pt3);	/* { dg-error "'ar3' not specified in enclosing 'target'" } */
}				/* { dg-error "'ag3' not specified in enclosing 'target'" "" { target *-*-* } .-1 } */
