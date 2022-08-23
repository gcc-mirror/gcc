/* PR target/99708 */
/* { dg-do compile } */

#ifdef __SIZEOF_FLOAT128__
__float128 f = 1.0;
#endif
long double l = 1.0;
