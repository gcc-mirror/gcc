/* Data definition with no type or storage class should receive a
   pedwarn, rather than a warning which becomes an error with
   -pedantic.  Test with no options.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "" } */

foo(); /* { dg-warning "data definition has no type or storage class" } */
