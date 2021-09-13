/* PR preprocessor/101638 */
/* { dg-do preprocess } */
/* { dg-options "-Wtraditional" } */

#define foo(attr) __has_attribute(attr)
#if foo(__deprecated__)
#endif
