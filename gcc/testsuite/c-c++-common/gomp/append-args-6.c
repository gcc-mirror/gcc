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



void g(int, const char *, int *, int *, omp_interop_t, omp_interop_t) { }
#pragma omp declare variant(g) match(construct={dispatch}) \
   append_args(interop(target,prefer_type( {fr("cuda") }, {fr(omp_ifr_hsa)} , {attr("ompx_a") } , {fr(omp_ifr_hip) }), targetsync), \
               interop(targetsync, prefer_type("cuda", "hsa"))) adjust_args(need_device_ptr : y, k)
void f(int x, const char *y, int *, int *k) { }


void gvar(int, const char *, int *, int *, omp_interop_t, omp_interop_t, ...) { }
#pragma omp declare variant(gvar) match(construct={dispatch}) \
   append_args(interop(target,prefer_type( {fr("cuda") }, {fr(omp_ifr_hsa)} , {attr("ompx_a") } , {fr(omp_ifr_hip) }), targetsync), \
               interop(targetsync, prefer_type("cuda", "hsa"))) adjust_args(need_device_ptr : y, k)
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




int *fi();

struct t {
  int *a, *b;
};

void fancy(int *x, int *y, omp_interop_t)  { }

#pragma omp declare variant(fancy) match(construct={dispatch}) adjust_args(need_device_ptr: x,y) \
                    append_args( interop (prefer_type(omp_ifr_hip), target) )
void bar(int *x, int *y);

void sub(struct t *s, void *y, const omp_interop_t obj5, omp_interop_t obj6)
{
 bar( fi(), s->b);

 // This is a bit questionable as dereferencing 's' as device pointer might not work (unspecified behavior);
 // but if for 's->b' it would still be need even if 's' was a device + host accessible pointer.
 #pragma omp dispatch device(3) is_device_ptr(s) interop(obj5)
    bar( fi(), s->b);

 bar( (int *) y, s->b);
 #pragma omp dispatch interop(obj6) is_device_ptr(y)
    bar( (int *) y, s->b);
}


/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = __builtin_omp_get_default_device \\(\\);" 4 "gimple" } }  */

/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = __builtin_omp_get_interop_int \\(obj6, -5, 0B\\);" 1 "gimple" } }  */

/* { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(5\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(4\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(3\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(D\.\[0-9\]+\\);" 5 "gimple" } }  */

/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(b, 5\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(cp2, 4\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(cp2, 4\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(cp2, 4\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(D\.\[0-9\]+, 3\\);" 2 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(D\.\[0-9\]+, D\.\[0-9\]+\\);" 1 "gimple" } }  */

/* { dg-final { scan-tree-dump-times "g \\(3, cp1, a, D\.\[0-9\]+, obj1, obj2\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "gvar \\(99, D\.\[0-9\]+, a, b, obj3, obj4, c, a, b, c, a, b, c\\);" 1 "gimple" } }  */

/* { dg-final { scan-tree-dump-times "fancy \\(D\.\[0-9\]+, D\.\[0-9\]+, obj5\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "fancy \\(y, D\.\[0-9\]+, obj6\\);" 1 "gimple" } }  */
