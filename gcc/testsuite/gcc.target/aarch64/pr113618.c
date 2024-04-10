/* { dg-do compile } */
/* { dg-options "-O2 -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */


/*
** move32:
**	...
**	ldp	q([0-9]+), q([0-9]+), \[x0\]
**	stp	q\1, q\2, \[x1\]
**	...
*/

void move32 (char *a, char *b)
{
    char temp[32];
    __builtin_memcpy (temp, a, 32);
    __builtin_memcpy (b, temp, 32);
}

/*
** move64:
**	...
**	ldp	q([0-9]+), q([0-9]+), \[x0\]
**	ldp	q([0-9]+), q([0-9]+), \[x0, 32\]
**	stp	q\1, q\2, \[x1\]
**	stp	q\3, q\4, \[x1, 32\]
**	...
*/

void move64 (char *a, char *b)
{
    char temp[64];
    __builtin_memcpy (temp, a, 64);
    __builtin_memcpy (b, temp, 64);
}
