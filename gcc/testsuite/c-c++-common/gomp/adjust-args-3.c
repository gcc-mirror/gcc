/* { dg-additional-options "-fdump-tree-gimple" } */

// Do diagnostic check / dump check only;
// Note: this test should work as run-test as well.

#if 0
  #include <omp.h>
#else
  #ifdef __cplusplus
  extern "C" {
  #endif
    extern int omp_get_default_device ();
    extern int omp_get_num_devices ();
  #ifdef __cplusplus
  }
  #endif
#endif


void f(int *x, int *y);
#pragma omp declare variant(f) adjust_args(need_device_ptr: x, y) match(construct={dispatch})
void g(int *x, int *y);

void
sub (int *a, int *b)
{
  // The has_device_addr is a bit questionable as the caller is not actually
  // passing a device address - but we cannot pass one because of the
  // following:
  //
  // As for 'b' need_device_ptr has been specified and 'b' is not
  // in the semantic requirement set 'is_device_ptr' (and only in 'has_device_addr')
  // "the argument is converted in the same manner that a use_device_ptr clause
  //  on a target_data construct converts its pointer"
  #pragma omp dispatch is_device_ptr(a), has_device_addr(b)  /* { dg-warning "'has_device_addr' for 'b' does not imply 'is_device_ptr' required for 'need_device_ptr' \\\[-Wopenmp\\\]" } */
    g(a, b);
}

void
f(int *from, int *to)
{
  static int cnt = 0;
  cnt++;
  if (cnt >= 3)
    {
      if (omp_get_default_device () != -1
          && omp_get_default_device () < omp_get_num_devices ())
        {
	  // On offload device but not mapped
	  if (from != (void *)0L) // Not mapped
	    __builtin_abort ();
        }
      else if (from[0] != 5)
        __builtin_abort ();
      return;
    }
  #pragma omp target is_device_ptr(from, to)
  {
    to[0] = from[0] * 10;
    to[1] = from[1] * 10;
  }
}

int
main ()
{
  int A[2], B[2] = {123, 456}, C[1] = {5};
  int *p = A;
  #pragma omp target enter data map(A, B)

  /* Note: We don't add  'use_device_addr(B)' here;
     if we do, it will fail with an illegal memory access (why?).  */
  #pragma omp target data use_device_ptr(p)
    {
      sub(p, B);
      sub(C, B); /* C is not mapped -> 'from' ptr == NULL  */
    }

  #pragma omp target exit data map(A, B)
}

// { dg-final { scan-tree-dump-times "#pragma omp dispatch has_device_addr\\(b\\) is_device_ptr\\(a\\)" 1 "gimple" } }
// { dg-final { scan-tree-dump-times "__builtin_omp_get_mapped_ptr" 1 "gimple" } }
// { dg-final { scan-tree-dump-times "D\\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(b" 1 "gimple" } }
// { dg-final { scan-tree-dump-times "f \\(a, D\\.\[0-9\]+\\);" 1 "gimple" } }
