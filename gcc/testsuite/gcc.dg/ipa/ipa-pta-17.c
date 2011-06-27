/* { dg-do compile } */
/* { dg-require-alias "" } */
/* { dg-options "-O -fipa-pta" } */

static int i;
extern int j __attribute__ ((alias ("i")));
int *p = &j;
