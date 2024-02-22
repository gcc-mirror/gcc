/* PR c/114007 */
/* { dg-do compile } */
/* { dg-options "-std=c11" } */

#if __has_c_attribute (gnu::unused)
[[gnu::unused]]
#endif
int i;
#if __has_cpp_attribute (gnu::unused)
[[gnu::unused]]
#endif
int j;
