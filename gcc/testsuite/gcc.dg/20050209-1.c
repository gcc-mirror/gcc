/* Test for infinite loop in parser error recovery.  From Serge
   Belyshev <ssb> on IRC.  */
/* { dg-do compile } */
/* { dg-options "" } */
int f() { return 1); } /* { dg-error "parse|syntax|expected" } */
