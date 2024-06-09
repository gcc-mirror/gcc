/* { dg-additional-options "-fexcess-precision=fast" } */

typedef struct __attribute__((__aligned__ (sizeof (void *)))) omp_depend_t {
  char __omp_depend_t__[2 * sizeof (void *)];
} omp_depend_t;

void
f ()
{
  omp_depend_t obj2;
  struct { omp_depend_t c; } s;
  float a;
  #pragma omp depobj(s.c) depend(inout: a)

  #pragma omp depobj(s.c) destroy(s.c) /* OK */

  #pragma omp depobj(s.c) destroy(obj2)
/* { dg-warning "the 'destroy' expression 'obj2' should be the same as the 'depobj' argument 's.c'" "" { target c } .-1 } */
/* { dg-warning "the 'destroy' expression 'obj2' should be the same as the 'depobj' argument 's.f\\(\\)::<unnamed struct>::c'" "" { target c++ } .-2 } */
}

void
g ()
{
  volatile omp_depend_t obj3;
  #pragma omp depobj(obj3) destroy(obj3)
}

int
main ()
{
   float a;
   omp_depend_t obj;

   #pragma omp depobj(obj) depend(inout: a)

   #pragma omp depobj(obj) destroy(obj) /* OK */

   #pragma omp depobj(obj) destroy(a + 5) 
/* { dg-error "'destroy' expression is not lvalue expression" "" { target c } .-1 } */
/* { dg-warning "the 'destroy' expression '\\(a \\+ \\(float\\)5\\)' should be the same as the 'depobj' argument 'obj'" "" { target c++ } .-2 } */

   #pragma omp depobj(obj+5) destroy(a) 
/* { dg-error "invalid operands to binary \\+ \\(have 'omp_depend_t' and 'int'\\)" "" { target c } .-1 } */
/* { dg-error "no match for 'operator\\+' in 'obj \\+ 5' \\(operand types are 'omp_depend_t' and 'int'\\)" "" { target c++ } .-2 } */

   #pragma omp depobj(obj) destroy(a)  /* { dg-warning "the 'destroy' expression 'a' should be the same as the 'depobj' argument 'obj'" } */
   return 0;
}
