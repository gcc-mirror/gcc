/* { dg-additional-options "-fdump-tree-gimple" } */

/* The following definitions are in omp_lib, which cannot be included
gcc/testsuite/g++.dg/gomp/append-args-1.C   in gcc/testsuite/  */

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


template<typename T, typename T2, typename T3>
void repl2(T, T2, T3, T3);
#pragma omp declare variant(repl2) match(construct={dispatch}) adjust_args(need_device_ptr : y) \
        append_args(interop(target, targetsync, prefer_type(1)), \
                    interop(target, prefer_type({fr(3), attr("ompx_nop")},{fr(2)},{attr("ompx_all")})))
template<typename T, typename T2>
void base2(T x, T2 y);



template<typename T, typename T2>
void repl3(T, T2, T2, T2, ...);
#pragma omp declare variant(repl3) match(construct={dispatch}) \
        append_args( interop(target, prefer_type("cuda", "hsa")), \
                     interop(targetsync), \
                     interop(target, prefer_type({attr("ompx_nop")})) )
template<typename T>
void base3(T, ...);




float
test (int *a, int *b)
{
  omp_interop_t obj1, obj2;
  float x, y;

  #pragma omp dispatch interop ( obj1 )
    base2<int *, int *> (b, a);

  #pragma omp dispatch nocontext(1)
    base3<int*>(a, 1, 2, "abc");

  #pragma omp dispatch
    base3<int*>(a, 1, 2, "abc");

  return x;
}

/* { dg-final { scan-tree-dump-times "base3<int\\*> \\(a, 1, 2, \"abc\"\\);" 1 "gimple" } }  */

/* { dg-final { scan-tree-dump-times "unsigned.* \\* interopobjs.\[0-9\]+\\\[1\\\];" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "unsigned.* \\* interopobjs.\[0-9\]+\\\[3\\\];" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "int tgt_tgtsync.\[0-9\]+\\\[1\\\];" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "int tgt_tgtsync.\[0-9\]+\\\[3\\\];" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "const char \\* pref_type.\[0-9\]+\\\[1\\\];" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "const char \\* pref_type.\[0-9\]+\\\[3\\\];" 1 "gimple" } }  */

/* { dg-final { scan-tree-dump-times "interopobjs.\[0-9\]+\\\[0\\\] = &interop\\.\[0-9\]+;" 2 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "interopobjs.\[0-9\]+\\\[1\\\] = &interop\\.\[0-9\]+;" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "interopobjs.\[0-9\]+\\\[2\\\] = &interop\\.\[0-9\]+;" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "tgt_tgtsync.\[0-9\]+\\\[0\\\] = 1;" 2 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "tgt_tgtsync.\[0-9\]+\\\[1\\\] = 2;" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "tgt_tgtsync.\[0-9\]+\\\[2\\\] = 1;" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "pref_type.\[0-9\]+\\\[0\\\] = \"\\\\x80\\\\x03\\\\x80ompx_nop\\\\x00\\\\x00\\\\x80\\\\x02\\\\x80\\\\x00\\\\x80\\\\x80ompx_all\\\\x00\\\\x00\";" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "pref_type.\[0-9\]+\\\[0\\\] = \"\\\\x80\\\\x01\\\\x80\\\\x00\\\\x80\\\\x07\\\\x80\\\\x00\";" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "pref_type.\[0-9\]+\\\[1\\\] = 0B;" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "pref_type.\[0-9\]+\\\[2\\\] = \"\\\\x80\\\\x80ompx_nop\\\\x00\\\\x00\";" 1 "gimple" } }  */


/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = __builtin_omp_get_interop_int \\(obj1, -5, 0B\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = __builtin_omp_get_default_device \\(\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(D\.\[0-9\]+\\);" 2 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(a, D\.\[0-9\]+\\);" 1 "gimple" } }  */

/* { dg-final { scan-tree-dump-times "__builtin_GOMP_interop \\(D\.\[0-9\]+, 1, &interopobjs\.\[0-9\], &tgt_tgtsync\.\[0-9\]+, &pref_type.2, 0, 0B, 0, 0B, 0, 0B\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "repl2<int\\*, int\\*, omp_interop_t> \\(b, D\.\[0-9\]+, obj1, interop\.\[0-9\]+\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_interop \\(D\.\[0-9\]+, 0, 0B, 0B, 0B, 0, 0B, 1, &interopobjs\.\[0-9\]+, 0, 0B\\);" 1 "gimple" } }  */


/* { dg-final { scan-tree-dump-times "__builtin_GOMP_interop \\(-5, 0, 0B, 0B, 0B, 0, 0B, 3, &interopobjs\.\[0-9\]+, 0, 0B\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "repl3<int\\*, omp_interop_t> \\(a, interop\.\[0-9\]+, interop\.\[0-9\]+, interop\.\[0-9\]+, 1, 2, \"abc\"\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_interop \\(-5, 3, &interopobjs\.\[0-9\]+, &tgt_tgtsync\.\[0-9\]+, &pref_type\.\[0-9\]+, 0, 0B, 0, 0B, 0, 0B\\);" 1 "gimple" } }  */
