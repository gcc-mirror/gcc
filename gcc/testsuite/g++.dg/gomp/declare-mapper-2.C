// { dg-do compile }

// Error-checking tests for "omp declare mapper".

struct S {
  int *ptr;
  int size;
};

struct Z {
  int z;
};

int main (int argc, char *argv[])
{
#pragma omp declare mapper (S v) map(v.size, v.ptr[:v.size]) // { dg-note "'#pragma omp declare mapper \\(S\\)' previously declared here" }

  /* This one's a duplicate.  */
#pragma omp declare mapper (default: S v) map (to: v.size) map (v) // { dg-error "redeclaration of '#pragma omp declare mapper \\(S\\)'" }

  /* ...and this one doesn't use a "base language identifier" for the mapper
     name.  */
#pragma omp declare mapper (case: S v) map (to: v.size) // { dg-error "expected identifier or 'default' before 'case'" }
  // { dg-error "expected ':' before 'case'" "" { target *-*-* } .-1 }

  /* A non-struct/class/union type isn't supposed to work.  */
#pragma omp declare mapper (name:Z [5]foo) map (foo[0].z) // { dg-error "'Z \\\[5\\\]' is not a struct, union or class type in '#pragma omp declare mapper'" }

  return 0;
}
