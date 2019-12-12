/* Verify that we include -Wno- variants when considering hints
   for misspelled options (PR driver/69453).  */

/* { dg-do compile } */
/* { dg-options "-fmo-unroll-loops" } */
/* { dg-error "unrecognized command-line option '-fmo-unroll-loops'; did you mean '-fno-unroll-loops'?"  "" { target *-*-* } 0 } */
