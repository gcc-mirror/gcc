/* { dg-do preprocess } */
/* { dg-options "" } */

#undef FOOBAR
#embed __FILE__ prefix (#include <non-existent-file.h>) suffix (#define FOOBAR baz) if_empty (#error "abcd")
#embed __FILE__ __prefix__(#define FOOBAR baz)
#ifdef FOOBAR
#error "FOOBAR is defined"
#endif
