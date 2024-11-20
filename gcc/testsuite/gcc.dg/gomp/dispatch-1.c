/* Test parsing of #pragma omp dispatch */
/* { dg-do compile } */

int f0 (int);

void f1 (void)
{
  int a, b;
  double x;
  struct {int a; int b;} s;
  int arr[1];

#pragma omp dispatch
  int c = f0 (a);	/* { dg-error "expected expression before 'int'" } */
#pragma omp dispatch
  int f2 (int d);	/* { dg-error "expected expression before 'int'" } */
#pragma omp dispatch
  a = b;	/* { dg-error "expected a function name before ';' token" } */
#pragma omp dispatch
  s.a = f0(a) + b;	/* { dg-error "expected ';' before '\\+' token" } */
#pragma omp dispatch
  b = !f0(a);	/* { dg-error "expected a function name before '!' token" } */
#pragma omp dispatch
  s.b += f0(s.a);	/* { dg-error "expected '=' before '\\+=' token" } */
#pragma omp dispatch
#pragma omp threadprivate(a)	/* { dg-error "expected expression before '#pragma'" } */
  a = f0(b);
  
#pragma omp dispatch nocontext(s) /* { dg-error "used struct type value where scalar is required" } */
  f0(a);
#pragma omp dispatch nocontext(a, b) /* { dg-error "expected '\\)' before ','" } */
  f0(a);
#pragma omp dispatch nocontext(a) nocontext(b) /* { dg-error "too many 'nocontext' clauses" } */
  f0(a);
#pragma omp dispatch novariants(s) /* { dg-error "used struct type value where scalar is required" } */
  f0(a);
#pragma omp dispatch novariants(a, b) /* { dg-error "expected '\\)' before ','" } */
  f0(a);
#pragma omp dispatch novariants(a) novariants(b) /* { dg-error "too many 'novariants' clauses" } */
  f0(a);
#pragma omp dispatch nowait nowait /* { dg-error "too many 'nowait' clauses" } */
  f0(a);
#pragma omp dispatch device(x) /* { dg-error "expected integer expression before end of line" } */
  f0(a);
#pragma omp dispatch device(arr) /* { dg-error "expected integer expression before end of line" } */
  f0(a);
#pragma omp dispatch is_device_ptr(x) /* { dg-error "'is_device_ptr' variable is neither a pointer nor an array" } */
  f0(a);
#pragma omp dispatch is_device_ptr(&x) /* { dg-error "expected identifier before '&' token" } */
  f0(a);
#pragma omp dispatch depend(inout: f0) /* { dg-error "'f0' is not lvalue expression nor array section in 'depend' clause" } */
  f0(a);
}
