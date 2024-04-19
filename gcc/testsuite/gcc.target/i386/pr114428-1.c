/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */
/* { dg-final { scan-assembler-times "psrlw" 1 } } */
/* { dg-final { scan-assembler-times "psrld" 1 } } */
/* { dg-final { scan-assembler-times "psrlq" 1 { target { ! ia32 } } } } */


#define SHIFTC 12

typedef int v4si __attribute__((vector_size(16)));
typedef short v8hi __attribute__((vector_size(16)));
typedef long long v2di __attribute__((vector_size(16)));

v8hi
foo1 (v8hi a)
{
  return
    (a >> (16 - SHIFTC)) & (__extension__(v8hi){(1<<SHIFTC)-1, (1<<SHIFTC)-1,
						(1<<SHIFTC)-1,(1<<SHIFTC)-1,
						(1<<SHIFTC)-1, (1<<SHIFTC)-1,
						(1<<SHIFTC)-1,(1<<SHIFTC)-1});
}

v4si
foo2 (v4si a)
{
  return
    (a >> (32 - SHIFTC)) & (__extension__(v4si){(1<<SHIFTC)-1, (1<<SHIFTC)-1,
						(1<<SHIFTC)-1,(1<<SHIFTC)-1});
}

v2di
__attribute__((target("avx512vl")))
foo3 (v2di a)
{
  return
    (a >> (long long)(64 - SHIFTC)) & (__extension__(v2di){(1ULL<<SHIFTC)-1,
							   (1ULL<<SHIFTC)-1});
}
