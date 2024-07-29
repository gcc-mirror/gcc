/* { dg-do compile } */

/* This used to ICE as comparisons on generic can be different types. */
/* PR middle-end/116134  */

int a;
int b;
int d;
void c() { 1UL <= (d < b) != (1UL & (0 < a | 0L)); }
