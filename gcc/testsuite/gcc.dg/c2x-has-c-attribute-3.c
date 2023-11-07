/* Test __has_c_attribute.  Test GNU attributes.  */
/* { dg-do preprocess } */
/* { dg-options "-std=c23 -pedantic-errors" } */

#if __has_c_attribute (gnu::packed) != 1
#error "bad result for gnu::packed"
#endif

#if __has_c_attribute (__gnu__::__packed__) != 1
#error "bad result for __gnu__::__packed__"
#endif

#if __has_c_attribute (gnu::__packed__) != 1
#error "bad result for gnu::__packed__"
#endif

#if __has_c_attribute (__gnu__::packed) != 1
#error "bad result for __gnu__::packed"
#endif

/* GNU attributes should not be reported as accepted without a scope
   specified.  */
#if __has_c_attribute (packed) != 0
#error "bad result for packed"
#endif
