/* Verify that we include -Wno- variants when considering hints
   for misspelled options (PR driver/69453).  */

/* { dg-do compile } */
/* { dg-options "-fno-if-convert" } */
/* { dg-error "unrecognized command-line option .-fno-if-convert.; did you mean .-fno-if-conversion.?"  "" { target *-*-* } 0 } */
