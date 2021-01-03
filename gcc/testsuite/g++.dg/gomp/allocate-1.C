// { dg-do compile }
// { dg-additional-options "-std=c++11" }

typedef enum omp_allocator_handle_t
#if __cplusplus >= 201103L
: __UINTPTR_TYPE__
#endif
{
  omp_null_allocator = 0,
  omp_default_mem_alloc = 1,
  omp_large_cap_mem_alloc = 2,
  omp_const_mem_alloc = 3,
  omp_high_bw_mem_alloc = 4,
  omp_low_lat_mem_alloc = 5,
  omp_cgroup_mem_alloc = 6,
  omp_pteam_mem_alloc = 7,
  omp_thread_mem_alloc = 8,
  __omp_allocator_handle_t_max__ = __UINTPTR_MAX__
} omp_allocator_handle_t;

namespace N1
{
  using ::omp_allocator_handle_t;
  void
  foo (const omp_allocator_handle_t h)
  {
    int x = 0;
    #pragma omp parallel allocate (h: x) private (x)
    x = 1;
  }
}

namespace N2
{
  typedef enum omp_allocator_handle_t { my = 0 } omp_allocator_handle_t;
  void
  foo (omp_allocator_handle_t h)
  {
    int x = 0;
    #pragma omp parallel allocate (h: x) private (x) // { dg-error "'allocate' clause allocator expression has type 'N2::omp_allocator_handle_t' rather than 'omp_allocator_handle_t'" }
    x = 1;
  }
}

struct S
{
  void foo ()
  {
    #pragma omp parallel allocate (omp_default_mem_alloc:s) firstprivate (s)
    s++;
  }
  int s;
};

template <typename T>
struct U
{
  int foo ()
  {
    #pragma omp parallel allocate (omp_default_mem_alloc:s) firstprivate (s)
    s++;
    return 1;
  }
  T s;
};

template <typename T>
int foo (T t)
{
  int x = 0;
  #pragma omp parallel firstprivate (x) allocate (t: x)
  x = 1;
  return 0;
}

template <typename T>
int bar (T t)
{
  int x = 0;
  #pragma omp parallel firstprivate (x) allocate (t: x)	// { dg-error "'allocate' clause allocator expression has type 'int' rather than 'omp_allocator_handle_t'" }
  x = 1;
  return 0;
}

omp_allocator_handle_t h;
int a = foo (h);
int b = bar (0);
int c = U<int> ().foo ();
