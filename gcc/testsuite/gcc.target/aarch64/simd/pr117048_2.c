/* { dg-do compile } */
/* { dg-options "-O2 -mlittle-endian" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

typedef char __attribute__ ((vector_size (16))) v16qi;
typedef unsigned short __attribute__ ((vector_size (16))) v8hi;
typedef unsigned int __attribute__ ((vector_size (16))) v4si;
typedef unsigned long long __attribute__ ((vector_size (16))) v2di;
typedef unsigned short __attribute__ ((vector_size (8))) v4hi;
typedef unsigned int __attribute__ ((vector_size (8))) v2si;

/*
** G1:
**	rev64	v0\.4s, v0\.4s
**	ret 
*/
v2di
G1 (v2di r)
{
  return (r >> 32) | (r << 32);
}

/*
** G2:
**	rev32	v0\.8h, v0\.8h
**	ret 
*/
v4si
G2 (v4si r)
{
  return (r >> 16) | (r << 16);
}

/*
** G3:
**	rev16	v0\.16b, v0\.16b
**	ret 
*/
v8hi
G3 (v8hi r)
{
  return (r >> 8) | (r << 8);
}

/*
** G4:
**	rev32	v0\.4h, v0\.4h
**	ret 
*/
v2si
G4 (v2si r)
{
  return (r >> 16) | (r << 16);
}

/*
** G5:
**	rev16	v0\.8b, v0\.8b
**	ret 
*/
v4hi
G5 (v4hi r)
{
  return (r >> 8) | (r << 8);
}

