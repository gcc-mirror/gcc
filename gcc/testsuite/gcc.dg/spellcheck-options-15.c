/* Verify that we don't offer -fsanitize=all as a suggestion for misspelled
   sanitizer options (PR driver/78877).  */

/* { dg-do compile } */
/* { dg-options "-fsanitize=al" } */
/* { dg-bogus "did you mean" "" { target *-*-* } 0 } */
/* { dg-error "unrecognized argument to -fsanitize= option: .al." "" { target *-*-* } 0 } */
