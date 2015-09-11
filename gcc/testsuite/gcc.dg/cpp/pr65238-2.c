/* PR preprocessor/65238 */
/* { dg-do preprocess } */

#if __has_attribute(
#endif
#if __has_attribute(unused
#endif
#if __has_attribute(unused, unused)
#endif
#if __has_attribute(__has_attribute(unused))
#endif

/* { dg-error "macro .__has_attribute. requires an identifier" "" {target "*-*-*"} 4 } */
/* { dg-error "missing ... after .__has_attribute." "" {target "*-*-*"} 6 } */
/* { dg-error "missing ... after .__has_attribute." "" {target "*-*-*"} 8 } */
/* { dg-error "missing binary operator before token .unused." "" {target "*-*-*"} 8 } */
/* { dg-error "macro .__has_attribute. requires an identifier" "" {target "*-*-*"} 10 } */
/* { dg-error "missing ... in expression" "" {target "*-*-*"} 10 } */
