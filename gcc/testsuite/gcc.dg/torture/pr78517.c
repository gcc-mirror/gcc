/* PR middle-end/78517 */
/* { dg-do compile } */
char a;
int fn1() { return a == '[' ? a : 0; }
