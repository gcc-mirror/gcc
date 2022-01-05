/* { dg-do compile } */

extern int a, b;
int c = &a == &a;
int d = &a != &b;
