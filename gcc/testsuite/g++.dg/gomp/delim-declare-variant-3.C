/* { dg-do compile } */

/* Check that "omp begin declare variant" for class methods outside of the
   class declaration gives a sorry.  C++ generally does not allow injection
   of additional methods into a class outside of its declaration so it is
   not clear what this is supposed to do.  */

class test1 {

 private:
  int n;
  static int m;

 public:

  void set_n (int x) { n = x; }
  int get_n (void) { return n; }
  
  static void set_m (int x) { m = x; }
  static int get_m (void) { return m; }

};

#pragma omp begin declare variant match (implementation={vendor("gnu")})
int test1::get_n (void) { return n * 2; }  /* { dg-message "sorry, unimplemented: cannot handle qualified name for variant function" } */
static int test1::get_m (void) { return m * 2; }  /* { dg-message "sorry, unimplemented: cannot handle qualified name for variant function" } */
#pragma omp end declare variant

int main (void)
{
  test1 t1;
  t1.set_n (10);
  if (t1.get_n () != 20) __builtin_abort ();
  test1::set_m (1);
  if (test1::get_m () != 2) __builtin_abort ();
}

