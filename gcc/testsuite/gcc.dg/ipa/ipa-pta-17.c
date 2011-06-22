/* { dg-do compile } */
/* { dg-options "-O -fipa-pta" } */

static int i;
extern int j __attribute__ ((alias ("i")));
int *p = &j;
