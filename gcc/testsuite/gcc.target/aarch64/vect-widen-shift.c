/* { dg-do compile } */
/* { dg-options "-O3 -save-temps" } */
/* { dg-final { check-function-bodies "**" "" "" } } */
#include <stdint.h>
#include <string.h>

#pragma GCC target "+nosve"

#define ARR_SIZE 1024

/* Should produce an shll,shll2 pair*/
/*
** sshll_opt1:
** 	...
** 	shll	v[0-9]+.4s, v[0-9]+.4h, 16
** 	shll2	v[0-9]+.4s, v[0-9]+.8h, 16
** 	...
*/
void sshll_opt1 (int32_t *foo, int16_t *a, int16_t *b)
{
    for( int i = 0; i < ARR_SIZE - 3;i=i+4)
    {
        foo[i]   = a[i]   << 16;
        foo[i+1] = a[i+1] << 16;
        foo[i+2] = a[i+2] << 16;
        foo[i+3] = a[i+3] << 16;
    }
}

/*
** sshll_opt2:
** 	...
** 	sxtl	v[0-9]+.4s, v[0-9]+.4h
** 	sxtl2	v[0-9]+.4s, v[0-9]+.8h
** 	sshl	v[0-9]+.4s, v[0-9]+.4s, v[0-9]+.4s
** 	sshl	v[0-9]+.4s, v[0-9]+.4s, v[0-9]+.4s
** 	...
*/
void sshll_opt2 (int32_t *foo, int16_t *a, int16_t *b)
{
    for( int i = 0; i < ARR_SIZE - 3;i=i+4)
    {
        foo[i]   = a[i]   << 16;
        foo[i+1] = a[i+1] << 15;
        foo[i+2] = a[i+2] << 14;
        foo[i+3] = a[i+3] << 17;
    }
}


