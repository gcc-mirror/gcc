/* Test multiple consecutive ## tokens.  */
/* { dg-do compile } */
/* { dg-options "" } */
#define cat(x,y) x##########y
int abcd;
int *p = &cat(ab,cd);
