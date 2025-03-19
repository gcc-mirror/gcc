/* { dg-additional-options "-fdump-tree-gimple" }  */

#if __cplusplus >= 201103L
# define __GOMP_UINTPTR_T_ENUM : __UINTPTR_TYPE__
#else
# define __GOMP_UINTPTR_T_ENUM
#endif

typedef enum omp_interop_t __GOMP_UINTPTR_T_ENUM
{
  omp_interop_none = 0,
  __omp_interop_t_max__ = __UINTPTR_MAX__
} omp_interop_t;

typedef enum omp_interop_fr_t
{
  omp_ifr_cuda = 1,
  omp_ifr_cuda_driver = 2,
  omp_ifr_opencl = 3,
  omp_ifr_sycl = 4,
  omp_ifr_hip = 5,
  omp_ifr_level_zero = 6,
  omp_ifr_hsa = 7,
  omp_ifr_last = omp_ifr_hsa
} omp_interop_fr_t;


extern int flag;

void g1(int, const char *, int *, int *, omp_interop_t, omp_interop_t) { }
void g2(int, const char *, int *, int *, omp_interop_t, omp_interop_t) { }
#pragma omp declare variant(g1) \
    match(construct={dispatch}, user={condition(flag==1)}) \
    append_args(interop(target,prefer_type( {fr("cuda") }, {fr(omp_ifr_hsa)} , {attr("ompx_a") } , {fr(omp_ifr_hip) }), targetsync), \
               interop(targetsync, prefer_type("cuda", "hsa"))) \
    adjust_args(need_device_ptr : y, k)
#pragma omp declare variant(g2) \
    match(construct={dispatch}, user={condition(flag==2)}) \
    append_args(interop(target,prefer_type( {fr("cuda") }, {fr(omp_ifr_hsa)} , {attr("ompx_a") } , {fr(omp_ifr_hip) }), targetsync), \
               interop(targetsync, prefer_type("cuda", "hsa"))) \
    adjust_args(need_device_ptr : y)
void f(int x, const char *y, int *, int *k) { }


void gvar1(int, const char *, int *, int *, omp_interop_t, omp_interop_t, ...) { }
void gvar2(int, const char *, int *, int *, omp_interop_t, omp_interop_t, ...) { }
#pragma omp declare variant(gvar1) \
   match(construct={dispatch}, user={condition(flag==3)}) \
   append_args(interop(target,prefer_type( {fr("cuda") }, {fr(omp_ifr_hsa)} , {attr("ompx_a") } , {fr(omp_ifr_hip) }), targetsync), \
               interop(targetsync, prefer_type("cuda", "hsa"))) \
   adjust_args(need_device_ptr : y, k)
#pragma omp declare variant(gvar2) \
   match(construct={dispatch}, user={condition(flag==4)}) \
   append_args(interop(target,prefer_type( {fr("cuda") }, {fr(omp_ifr_hsa)} , {attr("ompx_a") } , {fr(omp_ifr_hip) }), targetsync), \
               interop(targetsync, prefer_type("cuda", "hsa"))) \
   adjust_args(need_device_ptr : y)
void fvar(int x, const char *y, int *, int *k, ...) { }

void foo(const char *cp1, const char *cp2, int *a, int *b, int *c)
{
  omp_interop_t obj1, obj2, obj3, obj4;
  obj1 = obj2 = obj3 = obj4 = omp_interop_none;

  #pragma omp dispatch device(5) interop(obj1,obj2) is_device_ptr(cp1)
     f(3, cp1, a, b);

  #pragma omp dispatch device(4) interop(obj3,obj4) is_device_ptr(a,b,c)
     fvar(99, cp2, a, b, c, a, b, c, a, b, c);
}

/* Since the selectors are dynamic, there must be calls to all the variants as
   well as the base functions.  */
/* { dg-final { scan-tree-dump "f \\(3, cp1, a, b\\)" "gimple" } } */
/* { dg-final { scan-tree-dump "g1 \\(3, cp1, a, D\.\[0-9\]+, obj1, obj2\\)" "gimple" } } */
/* { dg-final { scan-tree-dump "g2 \\(3, cp1, a, b, obj1, obj2\\)" "gimple" } } */
/* { dg-final { scan-tree-dump "fvar \\(99, cp2, a, b, c, a, b, c, a, b, c\\)" "gimple" } } */
/* { dg-final { scan-tree-dump "gvar1 \\(99, D\.\[0-9\]+, a, b, obj3, obj4, c, a, b, c, a, b, c\\)" "gimple" } } */
/* { dg-final { scan-tree-dump "gvar2 \\(99, D\.\[0-9\]+, a, b, obj3, obj4, c, a, b, c, a, b, c\\)" "gimple" } } */

/* Check that the condition tests appear in the output, too.  */
/* { dg-final { scan-tree-dump "if \\(flag.\[0-9\]+ == 1\\)" "gimple" } } */
/* { dg-final { scan-tree-dump "if \\(flag.\[0-9\]+ == 2\\)" "gimple" } } */
/* { dg-final { scan-tree-dump "if \\(flag.\[0-9\]+ == 3\\)" "gimple" } } */
/* { dg-final { scan-tree-dump "if \\(flag.\[0-9\]+ == 4\\)" "gimple" } } */

/* There should be one call to map the pointer b for the call to f's variant
   g1; g2 doesn't have a need_device_ptr on this parameter.  */
/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(b," 1 "gimple" } }  */

/* Both variants for fvar need to map pointer cp2.  */
/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(cp2," 2 "gimple" } }  */



