/* Testcase for memory corruption bug in macro processing.
   See PR preprocessor/19077 for details.  */

/* { dg-do compile } */
/* { dg-options "-g3" } */
#define FOO(a,b,c,d,e) a b c d e \
"                                                                           " \
"                                                                           " \
"                                                                             "
int i;
