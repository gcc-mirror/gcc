// We use 'auto' without a function return type, so specify dialect here
// { dg-additional-options "-std=c++14 -fdump-tree-gimple" }
// { dg-skip-if "requires hosted libstdc++ for cstring" { ! hostedlib } }

#include <cstdlib>
#include <cstring>

struct T
{
  int *ptr;
  int ptr_len;

  int *&refptr;
  int refptr_len;

  auto set_ptr_func (int n)
  {
    auto fn = [=](void) -> bool
      {
	bool mapped;
	#pragma omp target map(from:mapped)
	{
	  if (ptr)
	    for (int i = 0; i < ptr_len; i++)
	      ptr[i] = n;
	  mapped = (ptr != NULL);
	}
	return mapped;
      };
    return fn;
  }

  auto set_refptr_func (int n)
  {
    auto fn = [=](void) -> bool
      {
	bool mapped;
	#pragma omp target map(from:mapped)
	{
	  if (refptr)
	    for (int i = 0; i < refptr_len; i++)
	      refptr[i] = n;
	  mapped = (refptr != NULL);
	}
	return mapped;
      };
    return fn;
  }
};

int main (void)
{
  #define N 10
  int *ptr1 = new int[N];
  int *ptr2 = new int[N];

  memset (ptr1, 0, sizeof (int) * N);
  memset (ptr2, 0, sizeof (int) * N);

  T a = { ptr1, N, ptr2, N };

  auto p1 = a.set_ptr_func (1);
  auto r2 = a.set_refptr_func (2);

  if (p1 ())
    abort ();
  if (r2 ())
    abort ();

  if (a.ptr != ptr1)
    abort ();
  if (a.refptr != ptr2)
    abort ();

  for (int i = 0; i < N; i++)
    if (ptr1[i] != 0)
      abort ();

  for (int i = 0; i < N; i++)
    if (ptr2[i] != 0)
      abort ();

  #pragma omp target data map(ptr1[:N], ptr2[:N])
  {
    if (!p1 ())
      abort ();
    if (!r2 ())
      abort ();
  }

  if (a.ptr != ptr1)
    abort ();
  if (a.refptr != ptr2)
    abort ();

  for (int i = 0; i < N; i++)
    if (ptr1[i] != 1)
      abort ();

  for (int i = 0; i < N; i++)
    if (ptr2[i] != 2)
      abort ();

  return 0;
}

/* { dg-final { scan-tree-dump {#pragma omp target num_teams.* firstprivate\(n\) map\(alloc:MEM.* \[len: 0\]\) map\(firstprivate:this \[pointer assign, bias: 0\]\) map\(to:\*__closure \[len: [0-9]+\]\) map\(firstprivate:__closure \[pointer assign, bias: 0\]\) map\(tofrom:\*_[0-9]+ \[len: [0-9]+\]\) map\(always_pointer:__closure->__this \[pointer assign, bias: 0\]\) map\(from:mapped \[len: 1\]\) map\(alloc:\*_[0-9]+ \[len: 0\]\) map\(attach_zero_length_array_section:_[0-9]+->ptr \[bias: 0\]\)} "gimple" } } */

/* { dg-final { scan-tree-dump {#pragma omp target num_teams.* firstprivate\(n\) map\(alloc:MEM.* \[len: 0\]\) map\(firstprivate:this \[pointer assign, bias: 0\]\) map\(to:\*__closure \[len: [0-9]+\]\) map\(firstprivate:__closure \[pointer assign, bias: 0\]\) map\(tofrom:\*_[0-9]+ \[len: [0-9]+\]\) map\(always_pointer:__closure->__this \[pointer assign, bias: 0\]\) map\(from:mapped \[len: [0-9]+\]\) map\(alloc:\*_[0-9]+ \[len: 0\]\) map\(alloc:\*_[0-9]+ \[pointer assign, zero-length array section, bias: 0\]\) map\(attach:_[0-9]+->refptr \[bias: 0\]\)} "gimple" } } */
