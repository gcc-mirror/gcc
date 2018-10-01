/* {dg-do compile} */
/* { dg-options "-Wabsolute-value" } */

int a;
int abs();
void b() { abs(a); }
