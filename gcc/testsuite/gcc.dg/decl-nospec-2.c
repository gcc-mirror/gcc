/* Data definition with no type or storage class should receive a
   pedwarn, rather than a warning which becomes an error with
   -pedantic.  Test with -pedantic.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu89 -pedantic" } */

foo(); /* { dg-warning "data definition has no type or storage class" } */
