/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -msse2" } */

typedef unsigned __int128 uv1ti __attribute__ ((__vector_size__ (16)));
typedef unsigned long uv2di __attribute__ ((__vector_size__ (16)));
typedef unsigned int uv4si __attribute__ ((__vector_size__ (16)));
typedef unsigned short uv8hi __attribute__ ((__vector_size__ (16)));
typedef unsigned char uv16qi __attribute__ ((__vector_size__ (16)));

uv1ti foo1(__int128 x) { return (uv1ti)x; }
uv2di foo2(__int128 x) { return (uv2di)x; }
uv4si foo4(__int128 x) { return (uv4si)x; }
uv8hi foo8(__int128 x) { return (uv8hi)x; }
uv16qi foo16(__int128 x) { return (uv16qi)x; }

/* { dg-final { scan-assembler-not "%\[er\]sp" } } */
