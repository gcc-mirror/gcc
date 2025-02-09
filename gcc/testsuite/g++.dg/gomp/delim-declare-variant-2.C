/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-gimple" } */

/* Check that "omp begin declare variant" works on methods in a 
   class declaration.  */

class test1 {

 private:
  int n;
  static int m;

 public:

  #pragma omp begin declare variant match (implementation={vendor("gnu")})
  int get_n (void) { return n * 2; }
  static int get_m (void) { return m * 2; }
  #pragma omp end declare variant

  #pragma omp begin declare variant match (construct={target})
  int get_n (void) { return this->n * 2; }
  #pragma omp end declare variant

  /* The base methods are deliberately declared after the variants in order
     to check that the lookup can still find them.  */
  void set_n (int x) { n = x; }
  int get_n (void) { return n; }
  
  static void set_m (int x) { m = x; }
  static int get_m (void) { return m; }
};

int test1::m;

int main (void)
{
  test1 t1;
  t1.set_n (10);
  if (t1.get_n () != 20) __builtin_abort ();
  test1::set_m (1);
  if (test1::get_m () != 2) __builtin_abort ();
}

/* { dg-final { scan-tree-dump "test1::get_n\\.ompvariant. \\(&t1\\)" "gimple" } } */
/* { dg-final { scan-tree-dump "test1::get_m\\.ompvariant. \\(\\)" "gimple" } } */

/* The variants must have internal linkage, not .globl or .weak.  */
/* { dg-final { scan-assembler-not "\\.globl\[ \t\]*_?_ZN5test117get_n\\.ompvariant" } } */
/* { dg-final { scan-assembler-not "\\.globl\[ \t\]*_?_ZN5test117get_m\\.ompvariant" } } */
/* { dg-final { scan-assembler-not "\\.weak\[ \t\]*_?_ZN5test117get_n\\.ompvariant" } } */
/* { dg-final { scan-assembler-not "\\.weak\[ \t\]*_?_ZN5test117get_m\\.ompvariant" } } */


