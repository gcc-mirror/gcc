/* { dg-do compile { target c++11 } } */
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


template<typename T2>
float repl0(T2, T2);
#pragma omp declare variant(repl0) match(construct={dispatch}) append_args(interop(target,prefer_type(1,5,4)), interop(targetsync))
float base0();



template<typename T, typename T2>
float repl1(T x, T2 y, T2 z) { return sizeof(x) + y == z; }
#pragma omp declare variant(repl1) match(construct={dispatch}) append_args(interop(target,prefer_type(1,5,4,sizeof(T))), interop(targetsync))
template<typename T>
float base1(T x) { return x + 42; }



template<typename T, typename T2, typename T3>
void repl3inval(T, T2, T3);
#pragma omp declare variant(repl3inval) match(construct={dispatch}) adjust_args(nothing : y) \
        append_args(interop(prefer_type({fr(3), attr("ompx_nop")},{fr(2)},{attr("ompx_all")}),target,targetsync))
template<typename T, typename T2>
void base2inval(T x, T2 y);



template<typename T>
void repl99(T);
#pragma omp declare variant(repl99) match(construct={dispatch}) \
        append_args(interop(target, targetsync, prefer_type("cuda")))
template<typename T>
void base99();



template<typename T, typename T2, typename T3>
void repl2(T, T2, T3, T3);
#pragma omp declare variant(repl2) match(construct={dispatch}) adjust_args(need_device_ptr : y) \
        append_args(interop(target, targetsync, prefer_type(1)), \
                    interop(target, targetsync, prefer_type({fr(3), attr("ompx_nop")},{fr(2)},{attr("ompx_all")})))
template<typename T, typename T2>
void base2(T x, T2 y);


omp_interop_t obj2, obj3;

void
test_it (char *str, int i, int *ip, float *fp, omp_interop_t obj1)
{
  #pragma omp dispatch interop(obj2, obj1) device(99)
    base0 ();

  float f2;
  #pragma omp dispatch interop(obj1, obj2) device(14)
    f2 = base1 (*fp);
  fp[0] = f2;

  #pragma omp dispatch interop(obj1)
    base2inval (str, i);

  #pragma omp dispatch interop(obj2) device(21)
    base99<double>();

  #pragma omp dispatch interop(obj3, obj1) device(31)
    base2(fp, ip);
}



/* { dg-final { scan-tree-dump-times "D\\.\[0-9\]+ = __builtin_omp_get_default_device \\(\\);" 5 "gimple" } }  */

/* { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(99\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(14\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(21\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(31\\);" 1 "gimple" } }  */

/* { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(D\\.\[0-9\]+\\);" 6 "gimple" } }  */

/* { dg-final { scan-tree-dump-times "D\\.\[0-9\]+ = __builtin_omp_get_interop_int \\(obj1, -5, 0B\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "D\\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(ip, 31\\);" 1 "gimple" } }  */

/* { dg-final { scan-tree-dump-times "repl0<omp_interop_t> \\(obj2\\.\[0-9\], obj1\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "f2 = repl1<float, omp_interop_t> \\(D\\.\[0-9\]+, obj1, obj2\\.\[0-9\]\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "repl3inval<char\\*, int, omp_interop_t> \\(str, i, obj1\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "repl99<omp_interop_t> \\(obj2\\.\[0-9\]\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "repl2<float\\*, int\\*, omp_interop_t> \\(fp, D\\.\[0-9\]+, obj3\\.\[0-9\], obj1\\);" 1 "gimple" } }  */
