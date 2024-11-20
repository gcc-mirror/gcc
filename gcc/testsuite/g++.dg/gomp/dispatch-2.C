/* Test parsing of #pragma omp dispatch */
/* { dg-do compile } */

struct S {
  int a;
  int b;
  virtual int f (double); 
};

int f0 (int);

void f1 (void)
{
  int a, b;
  double x;
  int arr[1];
  S s;

#pragma omp dispatch
  int c = f0 (a);	/* { dg-error "expected primary-expression before 'int'" } */
#pragma omp dispatch
  int f2 (int d);	/* { dg-error "expected primary-expression before 'int'" } */
#pragma omp dispatch
  a = b;	/* { dg-error "expected target-function call" } */
#pragma omp dispatch
  s.a = f0(a) + b;	/* { dg-error "expected ';' before '\\+' token" } */
#pragma omp dispatch
  b = !f0(a);	/* { dg-error "expected primary-expression before '!' token" } */
#pragma omp dispatch
  s.b += f0(s.a);	/* { dg-error "expected '=' before '\\+=' token" } */
#pragma omp dispatch
#pragma omp threadprivate(a)	/* { dg-error "'#pragma' is not allowed here" } */
  a = f0(b);
#pragma omp dispatch
  a = s.f(x);   /* { dg-error "'f' is a virtual function but only a direct call is allowed in a dispatch construct" } */
  
#pragma omp dispatch nocontext(s) /* { dg-error "could not convert 's' from 'S' to 'bool'" } */
  f0(a);
#pragma omp dispatch nocontext(a, b) /* { dg-error "expected '\\)' before ','" } */
  f0(a);
#pragma omp dispatch nocontext(a) nocontext(b) /* { dg-error "too many 'nocontext' clauses" } */
  f0(a);
#pragma omp dispatch novariants(s) /* { dg-error "could not convert 's' from 'S' to 'bool'" } */
  f0(a);
#pragma omp dispatch novariants(a, b) /* { dg-error "expected '\\)' before ','" } */
  f0(a);
#pragma omp dispatch novariants(a) novariants(b) /* { dg-error "too many 'novariants' clauses" } */
  f0(a);
#pragma omp dispatch nowait nowait /* { dg-error "too many 'nowait' clauses" } */
  f0(a);
#pragma omp dispatch device(x) /* { dg-error "'device' id must be integral" } */
  f0(a);
#pragma omp dispatch device(arr) /* { dg-error "'device' id must be integral" } */
  f0(a);
#pragma omp dispatch is_device_ptr(x) /* { dg-error "'is_device_ptr' variable is neither a pointer, nor an array nor reference to pointer" } */
  f0(a);
#pragma omp dispatch is_device_ptr(&x) /* { dg-error "expected unqualified-id before '&' token" } */
  f0(a);
#pragma omp dispatch depend(inout: s.f) /* { dg-error "'s.S::f' is not lvalue expression nor array section in 'depend' clause" } */
  f0(a);

}
