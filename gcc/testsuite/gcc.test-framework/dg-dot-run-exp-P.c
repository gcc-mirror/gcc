/* Test the tester; previously gcc.misc-tests/dg-11.c.  */
/* { dg-prms-id 42 } */
/* { dg-do run { target *-*-* } } */
/* The \n is left out of the pattern because tcl's exec will remove it.  */
/* { dg-output {Hello world[.]} } */

#include <stdio.h>

main () { printf ("Hello world.\n"); return 0; }
