/* PR preprocessor/65238 */
/* { dg-do preprocess } */
/* { dg-options "-traditional-cpp" } */

#if __has_attribute(
#endif
#if __has_attribute(unused
#endif
#if __has_attribute(unused, unused)
#endif
#if __has_attribute(__has_attribute(unused))
#endif

/* { dg-error "unterminated argument list invoking macro .__has_attribute." "" {target "*-*-*"} 5 } */
/* { dg-error "#if with no expression" "" {target "*-*-*"} 5 } */
/* { dg-error "unterminated argument list invoking macro .__has_attribute." "" {target "*-*-*"} 7 } */
/* { dg-error "macro .__has_attribute. passed 2 arguments, but takes just 1" "" {target "*-*-*"} 9 } */
/* { dg-error "missing ... in expression" "" {target "*-*-*"} 9 } */
/* { dg-error "macro .__has_attribute. requires an identifier" "" {target "*-*-*"} 11 } */
