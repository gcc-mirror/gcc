/* Verify that we include -Wno- variants when considering hints
   for misspelled options (PR driver/69453).  */

/* { dg-do compile } */
/* { dg-options "--Wno-narrowing" } */
/* { dg-error "unrecognized command line option '--Wno-narrowing'; did you mean '-Wno-narrowing'?"  "" { target *-*-* } 0 } */
