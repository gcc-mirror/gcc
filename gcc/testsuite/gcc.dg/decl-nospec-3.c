/* Data definition with no type or storage class should receive a
   pedwarn, rather than a warning which becomes an error with
   -pedantic.  Test with -pedantic-errors.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-pedantic-errors" } */

foo(); /* { dg-error "data definition has no type or storage class" } */
