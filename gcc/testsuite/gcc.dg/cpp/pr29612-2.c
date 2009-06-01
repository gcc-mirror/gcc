/* PR preprocessor/29612 */
/* { dg-do preprocess } */
/* { dg-options "-Wtraditional" } */

# 6 "pr29612-2.c"

#if 1U /* { dg-warning "traditional C rejects" "numeric constant suffix" } */
#endif

# 1 "foo.h" 1 3

#if 1U
#endif /* No warning in system header.  */

# 16 "pr29612-2.c" 2

#if 1U /* { dg-warning "traditional C rejects" "numeric constant suffix" } */
#endif
