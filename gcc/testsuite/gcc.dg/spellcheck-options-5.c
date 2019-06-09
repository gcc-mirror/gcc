/* Verify that we provide suggestions (with arguments) for the "-fno-"
   variant of "-fsanitize=" when it is misspelled (PR driver/69265).  */

/* { dg-do compile } */
/* { dg-options "-no-sanitize=all" } */
/* { dg-error "unrecognized command-line option '-no-sanitize=all'; did you mean '-fno-sanitize=all'?"  "" { target *-*-* } 0 } */
