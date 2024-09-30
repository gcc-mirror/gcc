/* { dg-do compile } */
/* { dg-additional-options "-O3" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#define FUNC(NAME, OPERATION, IMMEDIATE)   \
void NAME(int n) {                                      \
    for (int i = 0; i < n; i++)                         \
        out[i] = in[i] OPERATION IMMEDIATE;             \
}                                                       \

#define N 1024

int out[N], in[N];

/*
** foo:
**      ...
**      add	z[0-9]+.s, z[0-9]+.s, z[0-9]+.s
**      ...
*/
FUNC(foo, <<, 1)

/*
** foo2:
**      ...
**      lsl	z[0-9]+.s, z[0-9]+.s, #15
**      ...
*/
FUNC(foo2, <<, 15)

/*
** foo3:
**      ...
**      asr	z[0-9]+.s, z[0-9]+.s, #1
**      ...
*/
FUNC(foo3, >>, 1)

/*
** foo4:
**      ...
**      asr	z[0-9]+.s, z[0-9]+.s, #10
**      ...
*/
FUNC(foo4, >>, 10)
