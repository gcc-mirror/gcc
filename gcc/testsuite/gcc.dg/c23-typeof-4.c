/* Test C23 typeof and typeof_unqual on qualified arrays (bug 112841).  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

const int a[] = { 1, 2, 3 };
int b[3];
extern typeof (a) a;
extern typeof (const int [3]) a;
extern typeof_unqual (a) b;
extern typeof_unqual (const int [3]) b;
