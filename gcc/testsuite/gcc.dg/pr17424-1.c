/* Test for bug 17424: ICE for sizeof an undeclared variable.  */
/* { dg-do compile } */
/* { dg-options "" } */

;int foezis = sizeof tni; /* { dg-error "'tni' undeclared" } */
