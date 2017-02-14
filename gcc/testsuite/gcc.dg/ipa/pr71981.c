/* { dg-do compile } */
/* { dg-options "-O2 -w" } */

int **a;
static void fn1(char **p1) {
  char s = *p1, b = &s;
  while (*fn2()[a])
    ;
}
int main() { fn1(""); return 0; }
