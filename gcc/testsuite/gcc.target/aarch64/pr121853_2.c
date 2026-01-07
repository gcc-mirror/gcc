/* { dg-do compile } */
/* { dg-additional-options "-O1" } */
/* { dg-final { check-function-bodies "**" "" } } */

float convert(__bf16 value) {
    return (float)value;
}

/*
** convert:
**	movi	v[0-9]+.4s, 0
**	ext	v[0-9]+.16b, v[0-9]+.16b, v[0-9]+.16b, #14
**	ret
*/
