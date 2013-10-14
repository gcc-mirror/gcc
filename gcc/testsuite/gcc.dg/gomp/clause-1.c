/* { dg-do compile } */
/* { dg-require-effective-target tls } */

#define p parallel

extern void bar (void);
extern char q[];
int t;
#pragma omp threadprivate (t)

void
foo (int x)
{
  char *pp;
  struct S { int i; int j; } s;
  char a[32];
  double d;
  int i;
  const int c = 8;
#pragma omp p shared (x, x) /* { dg-error "more than once" } */
    ;
#pragma omp p private (x) private (x) /* { dg-error "more than once" } */
    ;
#pragma omp p shared (x) firstprivate (x) /* { dg-error "more than once" } */
    ;
#pragma omp p firstprivate (x, x) /* { dg-error "more than once" } */
    ;
#pragma omp p for shared (x) lastprivate (x) /* { dg-error "more than" } */
  for (i = 0; i < 10; i++)
    ;
#pragma omp p for private (x) lastprivate (x) /* { dg-error "more than" } */
  for (i = 0; i < 10; i++)
    ;
#pragma omp p for lastprivate (x, x) /* { dg-error "more than once" } */
  for (i = 0; i < 10; i++)
    ;
#pragma omp single private (x) copyprivate (x) /* { dg-error "more than" } */
    ;
#pragma omp p shared (bar) /* { dg-error "is not a variable" } */
    ;
#pragma omp p private (bar) /* { dg-error "is not a variable" } */
    ;
#pragma omp p firstprivate (bar) /* { dg-error "is not a variable" } */
    ;
#pragma omp p reduction (+:pp) /* { dg-error "user defined reduction not found for" } */
    ;
#pragma omp p reduction (*:s) /* { dg-error "user defined reduction not found for" } */
    ;
#pragma omp p reduction (-:a) /* { dg-error "user defined reduction not found for" } */
    ;
  d = 0;
#pragma omp p reduction (*:d)
    ;
#pragma omp p reduction (|:d) /* { dg-error "has invalid type for" } */
    ;
#pragma omp p reduction (&&:d) /* { dg-error "has invalid type for" } */
    ;
#pragma omp p copyin (d) /* { dg-error "must be 'threadprivate'" } */
    ;
#pragma omp p copyin (x) /* { dg-error "must be 'threadprivate'" } */
    ;
#pragma omp p for firstprivate (x) lastprivate (x)
  for (i = 0; i < 10; i++)
    ;
#pragma omp p private (q) /* { dg-error "incomplete type" } */
    ;
#pragma omp p firstprivate (q) /* { dg-error "incomplete type" } */
    ;
#pragma omp p for lastprivate (q) /* { dg-error "incomplete type" } */
  for (i = 0; i < 10; i++)
    ;
#pragma omp p shared (t) /* { dg-error "predetermined 'threadprivate'" } */
    ;
#pragma omp p private (t) /* { dg-error "predetermined 'threadprivate'" } */
    ;
#pragma omp p firstprivate (t) /* { dg-error "predetermined 'threadpriv" } */
    ;
#pragma omp p for lastprivate (t) /* { dg-error "predetermined 'threadpr" } */
  for (i = 0; i < 10; i++)
    ;
#pragma omp p reduction (*:t) /* { dg-error "predetermined 'threadprivate" } */
    ;
#pragma omp p shared (c) /* { dg-error "predetermined 'shared'" } */
    ;
#pragma omp p private (c) /* { dg-error "predetermined 'shared'" } */
    ;
#pragma omp p firstprivate (c)
    ;
#pragma omp p for lastprivate (c) /* { dg-error "predetermined 'shared'" } */
  for (i = 0; i < 10; i++)
    ;
#pragma omp p reduction (*:c) /* { dg-error "predetermined 'shared'" } */
    ;
}
