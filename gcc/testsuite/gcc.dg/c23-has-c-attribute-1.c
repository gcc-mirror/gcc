/* Test __has_c_attribute.  Test basic properties.  */
/* { dg-do preprocess } */
/* { dg-options "-std=c23 -pedantic-errors" } */

#ifdef __has_c_attribute
/* OK.  */
#else
#error "__has_c_attribute not defined"
#endif

#ifndef __has_c_attribute
#error "__has_c_attribute not defined"
#endif

#if defined __has_c_attribute
/* OK.  */
#else
#error "__has_c_attribute not defined"
#endif

#if __has_c_attribute(foo)
#error "foo attribute supported"
#endif

#if 0
#elif __has_c_attribute(foo)
#error "foo attribute supported"
#endif
