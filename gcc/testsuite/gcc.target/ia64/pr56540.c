/* PR target/56540 */
/* { dg-do compile } */

extern int a[__SIZEOF_FPREG__ != sizeof (__fpreg) ? -1 : 1];
extern int b[__SIZEOF_FLOAT80__ != sizeof (__float80) ? -1 : 1];
extern int c[__SIZEOF_FLOAT128__ != sizeof (__float128) ? -1 : 1];
