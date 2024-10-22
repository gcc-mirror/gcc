/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

typedef char __attribute__ ((vector_size (16))) v16qi;
typedef unsigned short __attribute__ ((vector_size (16))) v8hi;
typedef unsigned int __attribute__ ((vector_size (16))) v4si;
typedef unsigned long long __attribute__ ((vector_size (16))) v2di;
typedef char __attribute__ ((vector_size (8))) v8qi;
typedef unsigned short __attribute__ ((vector_size (8))) v4hi;
typedef unsigned int __attribute__ ((vector_size (8))) v2si;

#pragma GCC target "+sve2+sha3"

/*
** G1:
**	movi?	[vdz][0-9]+\.?(?:[0-9]*[bhsd])?, #?0
** 	xar	v0\.2d, v[0-9]+\.2d, v[0-9]+\.2d, 39
**      ret
*/
v2di
G1 (v2di r) {
    return (r >> 39) | (r << 25);
}

/*
** G2:
**	movi?	[vdz][0-9]+\.?(?:[0-9]*[bhsd])?, #?0
** 	xar	z0\.s, z[0-9]+\.s, z[0-9]+\.s, #23
**      ret
*/
v4si
G2 (v4si r) {
    return (r >> 23) | (r << 9);
}

/*
** G3:
**	movi?	[vdz][0-9]+\.?(?:[0-9]*[bhsd])?, #?0
** 	xar	z0\.h, z[0-9]+\.h, z[0-9]+\.h, #5
**      ret
*/
v8hi
G3 (v8hi r) {
    return (r >> 5) | (r << 11);
}

/*
** G4:
**	movi?	[vdz][0-9]+\.?(?:[0-9]*[bhsd])?, #?0
** 	xar	z0\.b, z[0-9]+\.b, z[0-9]+\.b, #6
**      ret
*/
v16qi
G4 (v16qi r)
{
  return (r << 2) | (r >> 6);
}

/*
** G5:
**	movi?	[vdz][0-9]+\.?(?:[0-9]*[bhsd])?, #?0
** 	xar	z0\.s, z[0-9]+\.s, z[0-9]+\.s, #22
**      ret
*/
v2si
G5 (v2si r) {
    return (r >> 22) | (r << 10);
}

/*
** G6:
**	movi?	[vdz][0-9]+\.?(?:[0-9]*[bhsd])?, #?0
** 	xar	z0\.h, z[0-9]+\.h, z[0-9]+\.h, #7
**      ret
*/
v4hi
G6 (v4hi r) {
    return (r >> 7) | (r << 9);
}

/*
** G7:
**	movi?	[vdz][0-9]+\.?(?:[0-9]*[bhsd])?, #?0
** 	xar	z0\.b, z[0-9]+\.b, z[0-9]+\.b, #5
**      ret
*/
v8qi
G7 (v8qi r)
{
  return (r << 3) | (r >> 5);
}

