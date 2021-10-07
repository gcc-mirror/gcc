/* { dg-do compile } */
/* { dg-options "-Os" } */

#include <stdlib.h>

/*
** cpy_127:
**      mov	(w|x)2, 127
**      b	memcpy
*/
void
cpy_127 (char *out, char *in)
{
  __builtin_memcpy (out, in, 127);
}

/*
** cpy_128:
**      mov	(w|x)2, 128
**      b	memcpy
*/
void
cpy_128 (char *out, char *in)
{
  __builtin_memcpy (out, in, 128);
}

/* { dg-final { check-function-bodies "**" "" "" } } */

