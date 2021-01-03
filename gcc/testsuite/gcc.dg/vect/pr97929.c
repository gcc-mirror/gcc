/* { dg-do compile } */
#include <stdint.h>
#define ARR_SIZE 1024
extern void foo (int32_t *bar, int16_t a)
{
    for( int i = 0; i < ARR_SIZE;i++)
    {
        bar[i]  = a + 1;
    }
}
