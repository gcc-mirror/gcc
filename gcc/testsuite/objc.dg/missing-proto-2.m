/* Test for graceful handling of missing protocol declarations.  */
/* Author: Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-do compile } */

void *protRef = @protocol(Missing); /* { dg-error "cannot find protocol declaration for .Missing." } */
