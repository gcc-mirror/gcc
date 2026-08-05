// { dg-do compile }
// { dg-additional-options "-fdump-tree-gimple" }
// { dg-skip-if "requires hosted libstdc++ for cstring" { ! hostedlib } }

#include <cstdlib>
#include <cstring>
extern "C" void abort ();

struct S
{
  int * ptr;
  int ptr_len;

  int *&refptr;
  int refptr_len;

  bool set_ptr (int n)
  {
    bool mapped;
    #pragma omp target map(from:mapped)
    {
      if (ptr != NULL)
	for (int i = 0; i < ptr_len; i++)
	  ptr[i] = n;
      mapped = (ptr != NULL);
    }
    return mapped;
  }

  bool set_refptr (int n)
  {
    bool mapped;
    #pragma omp target map(from:mapped)
    {
      if (refptr != NULL)
	for (int i = 0; i < refptr_len; i++)
	  refptr[i] = n;
      mapped = (refptr != NULL);
    }
    return mapped;
  }
};

int main (void)
{
  #define N 10
  int *ptr1 = new int[N];
  int *ptr2 = new int[N];

  memset (ptr1, 0, sizeof (int) * N);
  memset (ptr2, 0, sizeof (int) * N);

  S s = { ptr1, N, ptr2, N };

  bool mapped;
  int val = 123;

  mapped = s.set_ptr (val);
  if (mapped)
    abort ();
  if (s.ptr != ptr1)
    abort ();
  for (int i = 0; i < N; i++)
    if (ptr1[i] != 0)
      abort ();

  mapped = s.set_refptr (val);
  if (mapped)
    abort ();
  if (s.refptr != ptr2)
    abort ();
  for (int i = 0; i < N; i++)
    if (ptr2[i] != 0)
      abort ();

  #pragma omp target data map(ptr1[:N])
  mapped = s.set_ptr (val);

  if (!mapped)
    abort ();
  if (s.set_refptr (0))
    abort ();
  if (s.ptr != ptr1 || s.refptr != ptr2)
    abort ();
  for (int i = 0; i < N; i++)
    if (ptr1[i] != val)
      abort ();

  #pragma omp target data map(ptr2[:N])
  mapped = s.set_refptr (val);

  if (!mapped)
    abort ();
  if (s.set_ptr (0))
    abort ();
  if (s.ptr != ptr1 || s.refptr != ptr2)
    abort ();
  for (int i = 0; i < N; i++)
    if (ptr2[i] != val)
      abort ();

  return 0;
}

/* { dg-final { scan-tree-dump {#pragma omp target num_teams.* firstprivate\(n\) map\(tofrom:\*this \[len: [0-9]+\]\) map\(firstprivate:this \[pointer assign, bias: 0\]\) map\(from:mapped \[len: [0-9]+\]\) map\(alloc:\*_[0-9+] \[len: 0\]\) map\(alloc:\*_[0-9]+ \[pointer assign, zero-length array section, bias: 0\]\) map\(attach:this->refptr \[bias: 0\]\)} "gimple" } } */

/* { dg-final { scan-tree-dump {#pragma omp target num_teams.* firstprivate\(n\) map\(tofrom:\*this \[len: [0-9]+\]\) map\(firstprivate:this \[pointer assign, bias: 0\]\) map\(from:mapped \[len: [0-9]+\]\) map\(alloc:\*_[0-9]+ \[len: 0\]\) map\(attach_zero_length_array_section:this->ptr \[bias: 0\]\)} "gimple" } } */
