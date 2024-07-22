/* { dg-options "-O2" } */

#define vec8 __attribute__((vector_size(8)))
vec8 int f(int *a)
{
        asm("":"+w"(a));
        return (vec8 int){a[0], a[0]};
}
