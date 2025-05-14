/* { dg-additional-options "-fdump-tree-omplower" }  */

/* The following definitions are in omp_lib, which cannot be included
   in gcc/testsuite/  */

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

void
f()
{
  omp_interop_t obj1, obj2, obj3, obj4, obj5, obj6, obj7;
  int x[6];

  #pragma omp interop init (targetsync: obj1, obj2) use (obj3) destroy(obj4) init(target: obj5) destroy(obj6) use(obj7)   
  /* { dg-final { scan-tree-dump-times "void \\* interopobjs\.\[0-9\]+.3.;\[\r\n\ ]*int tgt_tgtsync\\.\[0-9\]+.3.;\[\r\n\ ]*void \\* interopobjs\\.\[0-9\]+.2.;\[\r\n\ ]*omp_interop_t obj3\\.\[0-9\]+;\[\r\n\ ]*void \\* obj3\\.\[0-9\]+;\[\r\n\ ]*omp_interop_t obj7\\.\[0-9\]+;\[\r\n\ ]*void \\* obj7\\.\[0-9\]+;\[\r\n\ ]*void \\* interopobjs\\.\[0-9\]+.2.;\[\r\n\ ]*interopobjs\.\[0-9\]+.0. = &obj1;\[\r\n \]*tgt_tgtsync\.\[0-9\]+.0. = 2;\[\r\n \]*interopobjs\.\[0-9\]+.1. = &obj2;\[\r\n \]*tgt_tgtsync\.\[0-9\]+.1. = 2;\[\r\n \]*interopobjs\.\[0-9\]+.2. = &obj5;\[\r\n \]*tgt_tgtsync\.\[0-9\]+.2. = 1;\[\r\n \]*obj3\.\[0-9\]+ = obj3;\[\r\n \]*obj3\.\[0-9\]+ = \\(void \\*\\) obj3\.\[0-9\]+;\[\r\n \]*interopobjs\.\[0-9\]+.0. = obj3\.\[0-9\]+;\[\r\n \]*obj7\.\[0-9\]+ = obj7;\[\r\n \]*obj7\.\[0-9\]+ = \\(void \\*\\) obj7\.\[0-9\]+;\[\r\n \]*interopobjs\.\[0-9\]+.1. = obj7\.\[0-9\]+;\[\r\n \]*interopobjs\.\[0-9\]+.0. = &obj4;\[\r\n \]*interopobjs\.\[0-9\]+.1. = &obj6;\[\r\n \]*__builtin_GOMP_interop \\(-5, 3, &interopobjs\.\[0-9\]+, &tgt_tgtsync\.\[0-9\]+, 0B, 2, &interopobjs\.\[0-9\]+, 2, &interopobjs\.\[0-9\]+, 0, 0B\\);" 1 "omplower" { target c } } }  */
  /* { dg-final { scan-tree-dump-times "void \\* interopobjs\.\[0-9\]+.3.;\[\r\n\ ]*int tgt_tgtsync\\.\[0-9\]+.3.;\[\r\n\ ]*void \\* interopobjs\\.\[0-9\]+.2.;\[\r\n\ ]*omp_interop_t obj3\\.\[0-9\]+;\[\r\n\ ]*void \\* obj3\\.\[0-9\]+;\[\r\n\ ]*omp_interop_t obj7\\.\[0-9\]+;\[\r\n\ ]*void \\* obj7\\.\[0-9\]+;\[\r\n\ ]*void \\* interopobjs\\.\[0-9\]+.2.;\[\r\n\ ]*omp_interop_t obj6\\.\[0-9\]+;\[\r\n\ ]*interopobjs\.\[0-9\]+.0. = &obj1;\[\r\n \]*tgt_tgtsync\.\[0-9\]+.0. = 2;\[\r\n \]*interopobjs\.\[0-9\]+.1. = &obj2;\[\r\n \]*tgt_tgtsync\.\[0-9\]+.1. = 2;\[\r\n \]*interopobjs\.\[0-9\]+.2. = &obj5;\[\r\n \]*tgt_tgtsync\.\[0-9\]+.2. = 1;\[\r\n \]*obj3\.\[0-9\]+ = obj3;\[\r\n \]*obj3\.\[0-9\]+ = \\(void \\*\\) obj3\.\[0-9\]+;\[\r\n \]*interopobjs\.\[0-9\]+.0. = obj3\.\[0-9\]+;\[\r\n \]*obj7\.\[0-9\]+ = obj7;\[\r\n \]*obj7\.\[0-9\]+ = \\(void \\*\\) obj7\.\[0-9\]+;\[\r\n \]*interopobjs\.\[0-9\]+.1. = obj7\.\[0-9\]+;\[\r\n \]*interopobjs\.\[0-9\]+.0. = &obj4;\[\r\n \]*obj6\.\[0-9\]+ = obj6;\[\r\n \]*interopobjs\.\[0-9\]+.1. = &obj6\.\[0-9\]+;\[\r\n \]*__builtin_GOMP_interop \\(-5, 3, &interopobjs\.\[0-9\]+, &tgt_tgtsync\.\[0-9\]+, 0B, 2, &interopobjs\.\[0-9\]+, 2, &interopobjs\.\[0-9\]+, 0, 0B\\);" 1 "omplower" { target c++ } } }  */

  #pragma omp interop nowait init (targetsync : obj1, obj2) use (obj3) destroy(obj4) init(target, targetsync : obj5) destroy(obj6) use(obj7) depend(inout: x)  
  /* { dg-final { scan-tree-dump-times "void \\* D\\.\[0-9\]+.3.;\[\r\n\ ]*void \\* interopobjs\\.\[0-9\]+.3.;\[\r\n\ ]*int tgt_tgtsync\\.\[0-9\]+.3.;\[\r\n\ ]*void \\* interopobjs\\.\[0-9\]+.2.;\[\r\n\ ]*omp_interop_t obj3\\.\[0-9\]+;\[\r\n\ ]*void \\* obj3\\.\[0-9\]+;\[\r\n\ ]*omp_interop_t obj7\\.\[0-9\]+;\[\r\n\ ]*void \\* obj7\\.\[0-9\]+;\[\r\n\ ]*void \\* interopobjs\\.\[0-9\]+.2.;\[\r\n\ ]*interopobjs\.\[0-9\]+.0. = &obj1;\[\r\n\ ]*tgt_tgtsync\.\[0-9\]+.0. = 2;\[\r\n\ ]*interopobjs\.\[0-9\]+.1. = &obj2;\[\r\n\ ]*tgt_tgtsync\.\[0-9\]+.1. = 2;\[\r\n\ ]*interopobjs\.\[0-9\]+.2. = &obj5;\[\r\n\ ]*tgt_tgtsync\.\[0-9\]+.2. = 3;\[\r\n\ ]*obj3\.\[0-9\]+ = obj3;\[\r\n\ ]*obj3\.\[0-9\]+ = \\(void \\*\\) obj3\.\[0-9\]+;\[\r\n\ ]*interopobjs\.\[0-9\]+.0. = obj3\.\[0-9\]+;\[\r\n\ ]*obj7\.\[0-9\]+ = obj7;\[\r\n\ ]*obj7\.\[0-9\]+ = \\(void \\*\\) obj7\.\[0-9\]+;\[\r\n\ ]*interopobjs\.\[0-9\]+.1. = obj7\.\[0-9\]+;\[\r\n\ ]*interopobjs\.\[0-9\]+.0. = &obj4;\[\r\n\ ]*interopobjs\.\[0-9\]+.1. = &obj6;\[\r\n\ ]*D\.\[0-9\]+.0. = 1B;\[\r\n\ ]*D\.\[0-9\]+.1. = 1B;\[\r\n\ ]*D\.\[0-9\]+.2. = &x;\[\r\n\ ]*__builtin_GOMP_interop \\(-5, 3, &interopobjs\.\[0-9\]+, &tgt_tgtsync\.\[0-9\]+, 0B, 2, &interopobjs\.\[0-9\]+, 2, &interopobjs\.\[0-9\]+, 1, &D\.\[0-9\]+\\);" 1 "omplower" { target c } } }  */
  /* { dg-final { scan-tree-dump-times "void \\* D\\.\[0-9\]+.3.;\[\r\n\ ]*void \\* interopobjs\\.\[0-9\]+.3.;\[\r\n\ ]*int tgt_tgtsync\\.\[0-9\]+.3.;\[\r\n\ ]*void \\* interopobjs\\.\[0-9\]+.2.;\[\r\n\ ]*omp_interop_t obj3\\.\[0-9\]+;\[\r\n\ ]*void \\* obj3\\.\[0-9\]+;\[\r\n\ ]*omp_interop_t obj7\\.\[0-9\]+;\[\r\n\ ]*void \\* obj7\\.\[0-9\]+;\[\r\n\ ]*void \\* interopobjs\\.\[0-9\]+.2.;\[\r\n\ ]*omp_interop_t obj6\\.\[0-9\]+;\[\r\n\ ]*interopobjs\.\[0-9\]+.0. = &obj1;\[\r\n\ ]*tgt_tgtsync\.\[0-9\]+.0. = 2;\[\r\n\ ]*interopobjs\.\[0-9\]+.1. = &obj2;\[\r\n\ ]*tgt_tgtsync\.\[0-9\]+.1. = 2;\[\r\n\ ]*interopobjs\.\[0-9\]+.2. = &obj5;\[\r\n\ ]*tgt_tgtsync\.\[0-9\]+.2. = 3;\[\r\n\ ]*obj3\.\[0-9\]+ = obj3;\[\r\n\ ]*obj3\.\[0-9\]+ = \\(void \\*\\) obj3\.\[0-9\]+;\[\r\n\ ]*interopobjs\.\[0-9\]+.0. = obj3\.\[0-9\]+;\[\r\n\ ]*obj7\.\[0-9\]+ = obj7;\[\r\n\ ]*obj7\.\[0-9\]+ = \\(void \\*\\) obj7\.\[0-9\]+;\[\r\n\ ]*interopobjs\.\[0-9\]+.1. = obj7\.\[0-9\]+;\[\r\n\ ]*interopobjs\.\[0-9\]+.0. = &obj4;\[\r\n\ ]*obj6\.\[0-9\]+ = obj6;\[\r\n\ ]*interopobjs\.\[0-9\]+.1. = &obj6\.\[0-9\]+;\[\r\n\ ]*D\.\[0-9\]+.0. = 1B;\[\r\n\ ]*D\.\[0-9\]+.1. = 1B;\[\r\n\ ]*D\.\[0-9\]+.2. = &x;\[\r\n\ ]*__builtin_GOMP_interop \\(-5, 3, &interopobjs\.\[0-9\]+, &tgt_tgtsync\.\[0-9\]+, 0B, 2, &interopobjs\.\[0-9\]+, 2, &interopobjs\.\[0-9\]+, 1, &D\.\[0-9\]+\\);" 1 "omplower" { target c++ } } }  */

  #pragma omp interop init (target: obj1, obj2) init (target: obj3) init(targetsync : obj4) init(target,targetsync: obj5)  
  /* { dg-final { scan-tree-dump-times "void \\* interopobjs\\.\[0-9\]+.5.;\[\r\n\ ]*int tgt_tgtsync\\.\[0-9\]+.5.;\[\r\n\ ]*interopobjs\.\[0-9\]+.0. = &obj1;\[\r\n \]*tgt_tgtsync\.\[0-9\]+.0. = 1;\[\r\n \]*interopobjs\.\[0-9\]+.1. = &obj2;\[\r\n \]*tgt_tgtsync\.\[0-9\]+.1. = 1;\[\r\n \]*interopobjs\.\[0-9\]+.2. = &obj3;\[\r\n \]*tgt_tgtsync\.\[0-9\]+.2. = 1;\[\r\n \]*interopobjs\.\[0-9\]+.3. = &obj4;\[\r\n \]*tgt_tgtsync\.\[0-9\]+.3. = 2;\[\r\n \]*interopobjs\.\[0-9\]+.4. = &obj5;\[\r\n \]*tgt_tgtsync\.\[0-9\]+.4. = 3;\[\r\n\ ]*__builtin_GOMP_interop \\(-5, 5, &interopobjs\.\[0-9\]+, &tgt_tgtsync\.\[0-9\]+, 0B, 0, 0B, 0, 0B, 0, 0B\\);" 1 "omplower" } }  */

  /* --------------------------------------------  */

  #pragma omp interop init (target, prefer_type(omp_ifr_cuda, omp_ifr_cuda+1, "hsa") : obj1, obj2) init (target: obj3) init(prefer_type(omp_ifr_hip, "sycl", omp_ifr_opencl), targetsync : obj4, obj7) init(target,prefer_type("level_zero", omp_ifr_level_zero+0),targetsync: obj5)  
  /* { dg-final { scan-tree-dump-times "void \\* interopobjs\\.\[0-9\]+.6.;\[\r\n\ ]*int tgt_tgtsync\\.\[0-9\]+.6.;\[\r\n\ ]*void \\* pref_type\.\[0-9\]+.6.;\[\r\n\ ]*interopobjs\.\[0-9\]+.0. = &obj1;\[\r\n \]*tgt_tgtsync\.\[0-9\]+.0. = 1;\[\r\n \]*pref_type\.\[0-9\]+.0. = .*;\[\r\n \]*interopobjs\.\[0-9\]+.1. = &obj2;\[\r\n \]*tgt_tgtsync\.\[0-9\]+.1. = 1;\[\r\n \]*pref_type\.\[0-9\]+.1. = .*;\[\r\n \]*interopobjs\.\[0-9\]+.2. = &obj3;\[\r\n \]*tgt_tgtsync\.\[0-9\]+.2. = 1;\[\r\n \]*pref_type\.\[0-9\]+.2. = 0B;\[\r\n \]*interopobjs\.\[0-9\]+.3. = &obj4;\[\r\n \]*tgt_tgtsync\.\[0-9\]+.3. = 2;\[\r\n \]*pref_type\.\[0-9\]+.3. = .*;\[\r\n \]*interopobjs\.\[0-9\]+.4. = &obj7;\[\r\n \]*tgt_tgtsync\.\[0-9\]+.4. = 2;\[\r\n \]*pref_type\.\[0-9\]+.4. = .*;\[\r\n \]*interopobjs\.\[0-9\]+.5. = &obj5;\[\r\n \]*tgt_tgtsync\.\[0-9\]+.5. = 3;\[\r\n \]*pref_type\.\[0-9\]+.5. = .*;\[\r\n\ ]*__builtin_GOMP_interop \\(-5, 6, &interopobjs\.\[0-9\]+, &tgt_tgtsync\.\[0-9\]+, &pref_type\.\[0-9\]+, 0, 0B, 0, 0B, 0, 0B\\);" 1 "omplower" } }  */


  /* -------------------------------------------- */

  #pragma omp interop init ( target, prefer_type( {fr("hip"), attr("ompx_gnu_prio:1", "ompx_gnu_debug")}, {attr("ompx_gnu_nicest"), attr("ompx_something")}) : obj1) destroy(obj3) nowait use(obj5)
  /* { dg-final { scan-tree-dump-times "void \\* interopobjs\\.\[0-9\]+.1.;\[\r\n\ ]*int tgt_tgtsync\\.\[0-9\]+.1.;\[\r\n\ ]*void \\* pref_type\.\[0-9\]+.1.;\[\r\n\ ]*void \\* interopobjs\.\[0-9\]+.1.;\[\r\n\ ]*omp_interop_t obj5\.\[0-9\]+;\[\r\n\ ]*void \\* obj5\.\[0-9\]+;\[\r\n\ ]*void \\* interopobjs\.\[0-9\]+.1.;\[\r\n\ ]*interopobjs\.\[0-9\]+.0. = &obj1;\[\r\n\ ]*tgt_tgtsync\.\[0-9\]+.0. = 1;\[\r\n\ ]*pref_type\.\[0-9\]+.0. = .*;\[\r\n\ ]*obj5\.\[0-9\]+ = obj5;\[\r\n\ ]*obj5\.\[0-9\]+ = \\(void \\*\\) obj5\.\[0-9\]+;\[\r\n\ ]*interopobjs\.\[0-9\]+.0. = obj5\.\[0-9\]+;\[\r\n\ ]*interopobjs\.\[0-9\]+.0. = &obj3;\[\r\n\ ]*__builtin_GOMP_interop \\(-5, 1, &interopobjs\.\[0-9\]+, &tgt_tgtsync\.\[0-9\]+, &pref_type\.\[0-9\]+, 1, &interopobjs\.\[0-9\]+, 1, &interopobjs\.\[0-9\]+, 1, 0B\\);" 1 "omplower" } }  */

}

void
g (int *y)
{
  omp_interop_t io1, io2, io3, io4, io5;

  [[omp::directive (interop,init(prefer_type({fr("level_zero")}, {fr(omp_ifr_sycl),attr("ompx_in_order"),attr("ompx_queue:in_order")}), targetsync : io1, io2),use(io3),destroy(io4,io5),depend(inout:y),nowait)]];  
  /* { dg-final { scan-tree-dump-times "void \\* D\.\[0-9\]+.3.;\[\r\n\ ]*void \\* interopobjs\.\[0-9\]+.2.;\[\r\n\ ]*int tgt_tgtsync\.\[0-9\]+.2.;\[\r\n\ ]*void \\* pref_type\.\[0-9\]+.2.;\[\r\n\ ]*void \\* interopobjs\.\[0-9\]+.1.;\[\r\n\ ]*void \\* io3\.\[0-9\]+;\[\r\n\ ]*void \\* interopobjs\.\[0-9\]+.2.;\[\r\n\ ]*interopobjs\.\[0-9\]+.0. = &io1;\[\r\n\ ]*tgt_tgtsync\.\[0-9\]+.0. = 2;\[\r\n\ ]*pref_type\.\[0-9\]+.0. = .*;\[\r\n\ ]*interopobjs\.\[0-9\]+.1. = &io2;\[\r\n\ ]*tgt_tgtsync\.\[0-9\]+.1. = 2;\[\r\n\ ]*pref_type\.\[0-9\]+.1. = .*;\[\r\n\ ]*io3\.\[0-9\]+ = \\(void \\*\\) io3;\[\r\n\ ]*interopobjs\.\[0-9\]+.0. = io3\.\[0-9\]+;\[\r\n\ ]*interopobjs\.\[0-9\]+.0. = &io4;\[\r\n\ ]*interopobjs\.\[0-9\]+.1. = &io5;\[\r\n\ ]*D\.\[0-9\]+.0. = 1B;\[\r\n\ ]*D\.\[0-9\]+.1. = 1B;\[\r\n\ ]*D\.\[0-9\]+.2. = &y;\[\r\n\ ]*__builtin_GOMP_interop \\(-5, 2, &interopobjs\.\[0-9\]+, &tgt_tgtsync\.\[0-9\]+, &pref_type\.\[0-9\]+, 1, &interopobjs\.\[0-9\]+, 2, &interopobjs\.\[0-9\]+, 1, &D\.\[0-9\]+\\);" 1 "omplower" { target c } } }  */
  /* { dg-final { scan-tree-dump-times "void \\* D\.\[0-9\]+.3.;\[\r\n\ ]*void \\* interopobjs\.\[0-9\]+.2.;\[\r\n\ ]*int tgt_tgtsync\.\[0-9\]+.2.;\[\r\n\ ]*void \\* pref_type\.\[0-9\]+.2.;\[\r\n\ ]*void \\* interopobjs\.\[0-9\]+.1.;\[\r\n\ ]*void \\* io3\.\[0-9\]+;\[\r\n\ ]*void \\* interopobjs\.\[0-9\]+.2.;\[\r\n\ ]*omp_interop_t io4\.\[0-9\]+;\[\r\n\ ]*omp_interop_t io5\.\[0-9\]+;\[\r\n\ ]*interopobjs\.\[0-9\]+.0. = &io1;\[\r\n\ ]*tgt_tgtsync\.\[0-9\]+.0. = 2;\[\r\n\ ]*pref_type\.\[0-9\]+.0. = .*;\[\r\n\ ]*interopobjs\.\[0-9\]+.1. = &io2;\[\r\n\ ]*tgt_tgtsync\.\[0-9\]+.1. = 2;\[\r\n\ ]*pref_type\.\[0-9\]+.1. = .*;\[\r\n\ ]*io3\.\[0-9\]+ = \\(void \\*\\) io3;\[\r\n\ ]*interopobjs\.\[0-9\]+.0. = io3\.\[0-9\]+;\[\r\n\ ]*io4\.\[0-9\]+ = io4;\[\r\n\ ]*interopobjs\.\[0-9\]+.0. = &io4\.\[0-9\]+;\[\r\n\ ]*io5\.\[0-9\]+ = io5;\[\r\n\ ]*interopobjs\.\[0-9\]+.1. = &io5\.\[0-9\]+;\[\r\n\ ]*D\.\[0-9\]+.0. = 1B;\[\r\n\ ]*D\.\[0-9\]+.1. = 1B;\[\r\n\ ]*D\.\[0-9\]+.2. = &y;\[\r\n\ ]*__builtin_GOMP_interop \\(-5, 2, &interopobjs\.\[0-9\]+, &tgt_tgtsync\.\[0-9\]+, &pref_type\.\[0-9\]+, 1, &interopobjs\.\[0-9\]+, 2, &interopobjs\.\[0-9\]+, 1, &D\.\[0-9\]+\\);" 1 "omplower" { target c++ } } }  */
}
