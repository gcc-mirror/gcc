/* { dg-do compile } */
/* { dg-options "-mavx512f -mno-avx512vl -mprefer-vector-width=512 -Os" } */

typedef int v16si __attribute__ ((vector_size (64)));
typedef long long v8di __attribute__((vector_size (64)));

v16si foo_v16si (const int *a)
{
    return (__extension__ (v16si) {~*a, ~*a, ~*a, ~*a, ~*a, ~*a, ~*a, ~*a,
				   ~*a, ~*a, ~*a, ~*a, ~*a, ~*a, ~*a, ~*a});
}

v8di foo_v8di (const long long *a)
{
    return (__extension__ (v8di) {~*a, ~*a, ~*a, ~*a, ~*a, ~*a, ~*a, ~*a});
}

/* { dg-final { scan-assembler-times "vpternlog\[dq\]\[ \\t\]+\\\$0x55, \\(%(?:eax|rdi|edi)\\)\\\{1to\[1-8\]+\\\}" 2 } } */
