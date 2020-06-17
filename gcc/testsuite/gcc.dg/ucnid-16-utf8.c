/* { dg-do compile } */
/* { dg-require-iconv "latin1" } */
/* { dg-options "-std=c99 -g -finput-charset=latin1" } */
/* { dg-final { scan-file ucnid-16-utf8.s "Â²" } } */

/* This superscript is encoded in latin1; verify that we still get UTF-8 in the output.  */
int x² = 9;
