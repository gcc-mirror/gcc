// { dg-do compile }

// Error-checking tests for "omp declare mapper".

typedef struct {
  int *ptr;
  int size;
} S;

typedef struct {
  int z;
} Z;

int main (int argc, char *argv[])
{
#pragma omp declare mapper (S v) map(v.size, v.ptr[:v.size])
/* { dg-error "previous '#pragma omp declare mapper'" "" { target c } .-1 } */

  /* This one's a duplicate.  */
#pragma omp declare mapper (default: S v) map (to: v.size) map (v)
/* { dg-error "redeclaration of '<default>' '#pragma omp declare mapper' for type 'S'" "" { target c } .-1 } */

  /* ...and this one doesn't use a "base language identifier" for the mapper
     name.  */
#pragma omp declare mapper (case: S v) map (to: v.size)
/* { dg-error "expected identifier or 'default'" "" { target c } .-1 } */

  /* A non-struct/class/union type isn't supposed to work.  */
#pragma omp declare mapper (name:Z [5]foo) map (foo[0].z)
/* { dg-error "'Z\\\[5\\\]' is not a struct or union type in '#pragma omp declare mapper'" "" { target c } .-1 } */

  return 0;
}
