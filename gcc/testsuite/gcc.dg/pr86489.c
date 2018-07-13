
/* { dg-do compile } */
/* { dg-options "-O2" } */

int a = 0, b = 0;
void fn1() {
    int c = 0;
    for (; a; a--)
      c += b;
    while ((c - 1) & c)
      ;
}
