/* Verify that we provide simple suggestions for the arguments of
   "-fsanitize-recover=" when it is misspelled (PR driver/69265).  */

/* { dg-do compile } */
/* { dg-options "-sanitize-recover=integer-divide-by-0" } */
/* { dg-error "unrecognized command line option '-sanitize-recover=integer-divide-by-0'; did you mean '-fsanitize-recover=integer-divide-by-zero'?"  "" { target *-*-* } 0 } */
