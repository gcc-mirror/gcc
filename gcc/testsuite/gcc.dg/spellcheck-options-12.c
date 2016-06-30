/* Verify that we don't include -Wno- variants for options marked
   with RejectNegative when considering hints for misspelled options
   (PR driver/71651).  */

/* { dg-do compile } */
/* { dg-options "-fno-stack-protector-explicit" } */
/* { dg-error "unrecognized command line option .-fno-stack-protector-explicit.; did you mean .-fstack-protector-explicit.." "" { target *-*-* } 0 } */
