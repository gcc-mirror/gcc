/* PR c/69819 */
/* { dg-do compile } */

void foo () { }
int foo[] = { 0 }; /* { dg-error ".foo. redeclared as different kind of symbol" } */
