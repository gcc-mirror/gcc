/* Verify that we provide a hint if the user misspells an option that
   takes an argument (PR driver/69265).  */

/* { dg-do compile } */
/* { dg-options "-tls-model=global-dynamic" } */
/* { dg-error "unrecognized command line option '-tls-model=global-dynamic'; did you mean '-ftls-model=global-dynamic'?"  "" { target *-*-* } 0 } */
