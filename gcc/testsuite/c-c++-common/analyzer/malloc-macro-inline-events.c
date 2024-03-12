/* Test path-printing in the face of macros.  */

/* { dg-additional-options "-fdiagnostics-show-line-numbers -fdiagnostics-path-format=inline-events -fdiagnostics-show-caret" } */
/* { dg-enable-nn-line-numbers "" } */

#include "malloc-macro.h"

/* { dg-warning "double-'free' of 'ptr'" "" { target *-*-* } 2 } */

void test (void *ptr)
{
  WRAPPED_FREE (ptr); /* { dg-message "in expansion of macro 'WRAPPED_FREE'" } */
  WRAPPED_FREE (ptr); /* { dg-message "in expansion of macro 'WRAPPED_FREE'" } */

  /* { dg-begin-multiline-output "" }
   NN | #define WRAPPED_FREE(PTR) free(PTR)
      |                           ^~~~~~~~~
   NN |   WRAPPED_FREE (ptr);
      |   ^~~~~~~~~~~~
  'test': event 1
    |
    |
    |   NN | #define WRAPPED_FREE(PTR) free(PTR)
    |      |                           ^~~~~~~~~
    |      |                           |
    |      |                           (1) first 'free' here
    |   NN |   WRAPPED_FREE (ptr);
    |      |   ^~~~~~~~~~~~
    |
  'test': event 2
    |
    |
    |   NN | #define WRAPPED_FREE(PTR) free(PTR)
    |      |                           ^~~~~~~~~
    |      |                           |
    |      |                           (2) second 'free' here; first 'free' was at (1)
    |   NN |   WRAPPED_FREE (ptr);
    |      |   ^~~~~~~~~~~~
    |
     { dg-end-multiline-output "" { target c } } */
  /* { dg-begin-multiline-output "" }
   NN | #define WRAPPED_FREE(PTR) free(PTR)
      |                           ~~~~^~~~~
   NN |   WRAPPED_FREE (ptr);
      |   ^~~~~~~~~~~~
  'void test(void*)': event 1
    |
    |
    |   NN | #define WRAPPED_FREE(PTR) free(PTR)
    |      |                           ~~~~^~~~~
    |      |                               |
    |      |                               (1) first 'free' here
    |   NN |   WRAPPED_FREE (ptr);
    |      |   ^~~~~~~~~~~~
    |
  'void test(void*)': event 2
    |
    |
    |   NN | #define WRAPPED_FREE(PTR) free(PTR)
    |      |                           ~~~~^~~~~
    |      |                               |
    |      |                               (2) second 'free' here; first 'free' was at (1)
    |   NN |   WRAPPED_FREE (ptr);
    |      |   ^~~~~~~~~~~~
    |
     { dg-end-multiline-output "" { target c++ } } */
}
