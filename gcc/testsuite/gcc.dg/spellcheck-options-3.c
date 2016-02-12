/* Verify that we provide simple suggestions for the arguments of
   "-fsanitize=" when it is misspelled (PR driver/69265).  */

/* { dg-do compile } */
/* { dg-options "-sanitize=address" } */
/* { dg-error "unrecognized command line option '-sanitize=address'; did you mean '-fsanitize=address'?"  "" { target *-*-* } 0 } */
