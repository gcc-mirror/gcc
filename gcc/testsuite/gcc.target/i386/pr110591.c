/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mcmpccxadd -O2" } */
/* { dg-final { scan-assembler-not {cmp[lq]?[ \t]+} } } */
/* { dg-final { scan-assembler-times {cmpoxadd[ \t]+} 12 } } */

#include <immintrin.h>

_Bool foo_setg (int *ptr, int v)
{
    return _cmpccxadd_epi32(ptr, v, 1, _CMPCCX_O) > v;
}

_Bool foo_setl (int *ptr, int v)
{
    return _cmpccxadd_epi32(ptr, v, 1, _CMPCCX_O) < v;
}

_Bool foo_sete(int *ptr, int v)
{
    return _cmpccxadd_epi32(ptr, v, 1, _CMPCCX_O) == v;
}

_Bool foo_setne(int *ptr, int v)
{
    return _cmpccxadd_epi32(ptr, v, 1, _CMPCCX_O) != v;
}

_Bool foo_setge(int *ptr, int v)
{
    return _cmpccxadd_epi32(ptr, v, 1, _CMPCCX_O) >= v;
}

_Bool foo_setle(int *ptr, int v)
{
    return _cmpccxadd_epi32(ptr, v, 1, _CMPCCX_O) <= v;
}

_Bool fooq_setg (long long *ptr, long long v)
{
    return _cmpccxadd_epi64(ptr, v, 1, _CMPCCX_O) > v;
}

_Bool fooq_setl (long long *ptr, long long v)
{
    return _cmpccxadd_epi64(ptr, v, 1, _CMPCCX_O) < v;
}

_Bool fooq_sete(long long *ptr, long long v)
{
    return _cmpccxadd_epi64(ptr, v, 1, _CMPCCX_O) == v;
}

_Bool fooq_setne(long long *ptr, long long v)
{
    return _cmpccxadd_epi64(ptr, v, 1, _CMPCCX_O) != v;
}

_Bool fooq_setge(long long *ptr, long long v)
{
    return _cmpccxadd_epi64(ptr, v, 1, _CMPCCX_O) >= v;
}

_Bool fooq_setle(long long *ptr, long long v)
{
    return _cmpccxadd_epi64(ptr, v, 1, _CMPCCX_O) <= v;
}
