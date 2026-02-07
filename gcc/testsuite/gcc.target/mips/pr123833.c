/* { dg-do compile } */
/* { dg-options "-O1 -mabi=64 -mhard-float" } */

typedef short int16_t;
typedef signed char int8_t;
typedef long long int64_t;
typedef unsigned char uint8_t;
typedef unsigned long long uint64_t;

#define BS_VEC(type, num) type __attribute__((vector_size(num * sizeof(type))))
BS_VEC(int8_t, 2) backsmith_pure_0(int64_t);
uint64_t backsmith_pure_4(int16_t BS_ARG_0)
{
    BS_VEC(int16_t, 16)
    BS_VAR_0 = __builtin_convertvector(
        __builtin_shufflevector(backsmith_pure_0(0), backsmith_pure_0(0), 0, 2,
                                2, 3, 2, 1, 2, 3, 0, 2, 0, 0, 2, 2, 0, 1),
        BS_VEC(int16_t, 16));
    for (;;)
    {
        if (__builtin_ctzg((uint8_t)BS_VAR_0[3], BS_ARG_0))
            for (;;);
        return 0;
    }
}

