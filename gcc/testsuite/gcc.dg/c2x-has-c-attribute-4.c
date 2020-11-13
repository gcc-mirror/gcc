/* Test __has_c_attribute.  Test syntax errors.  */
/* { dg-do preprocess } */
/* { dg-options "-std=c2x -pedantic-errors" } */

#if __has_c_attribute /* { dg-error "missing '\\('" } */
#endif

#if __has_c_attribute 0 /* { dg-error "missing '\\('" } */
#endif

#if __has_c_attribute (0 /* { dg-error "requires an identifier" } */
#endif

#if __has_c_attribute (x /* { dg-error "missing '\\)'" } */
#endif

#if __has_c_attribute (x::0) /* { dg-error "required after scope" } */
#endif
