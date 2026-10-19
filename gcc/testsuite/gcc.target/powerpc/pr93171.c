/* { dg-do compile { target powerpc64*-*-* } } */
/* { dg-options "-O2 -m64 -mcpu=power10" } */

typedef unsigned long long uint64_t;

typedef union {
    uint64_t u64;
    struct {
        uint64_t op   : 4;
        uint64_t type : 3;
        uint64_t tag  : 32;
    } s;
} TagReq;

__attribute__((noinline, noclone))
void foo(uint64_t *a, int type, int tag)
{
    TagReq req;
    req.u64 = 0;

    req.s.tag  = (unsigned)tag;
    req.s.type = (unsigned)type;

    *a = req.u64;
}

/* Expect exactly one rldic and one rldimi.  */
 /* { dg-final { scan-assembler-times {\mrldic(\.)?\M} 1 } } */
 /* { dg-final { scan-assembler-times {\mrldimi(\.)?\M} 1 } } */
