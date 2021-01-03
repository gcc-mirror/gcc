/* { dg-do run } */
/* { dg-options "-O3 -save-temps" } */
#include <stdint.h>
#include <string.h>

#pragma GCC target "+nosve"

#define ARR_SIZE 1024

/* Should produce an shll,shll2 pair*/
void sshll_opt (int32_t *foo, int16_t *a, int16_t *b)
{
    for( int i = 0; i < ARR_SIZE - 3;i=i+4)
    {
        foo[i]   = a[i]   << 16;
        foo[i+1] = a[i+1] << 16;
        foo[i+2] = a[i+2] << 16;
        foo[i+3] = a[i+3] << 16;
    }
}

__attribute__((optimize (0)))
void sshll_nonopt (int32_t *foo, int16_t *a, int16_t *b)
{
    for( int i = 0; i < ARR_SIZE - 3;i=i+4)
    {
        foo[i]   = a[i]   << 16;
        foo[i+1] = a[i+1] << 16;
        foo[i+2] = a[i+2] << 16;
        foo[i+3] = a[i+3] << 16;
    }
}


void __attribute__((optimize (0)))
init(uint16_t *a, uint16_t *b)
{
    for( int i = 0; i < ARR_SIZE;i++)
    {
      a[i] = i;
      b[i] = 2*i;
    }
}

int __attribute__((optimize (0)))
main()
{
    uint32_t foo_arr[ARR_SIZE];
    uint32_t bar_arr[ARR_SIZE];
    uint16_t a[ARR_SIZE];
    uint16_t b[ARR_SIZE];

    init(a, b);
    sshll_opt(foo_arr, a, b);
    sshll_nonopt(bar_arr, a, b);
    if (memcmp(foo_arr, bar_arr, ARR_SIZE) != 0)
      return 1;
    return 0;
}

/* { dg-final { scan-assembler-times {\tshll\t} 1} } */
/* { dg-final { scan-assembler-times {\tshll2\t} 1} } */
