/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic" } */

char d[(_Bool)0.5 == 1 ? 1 : -1];
char f[(_Bool)0.0 == 0 ? 1 : -1];
