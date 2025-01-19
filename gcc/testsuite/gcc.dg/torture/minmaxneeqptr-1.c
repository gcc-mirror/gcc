/* { dg-do compile } */

/* PR tree-optimization/117646 */

int maxeq(char *a, char *b) {
  char *p = a < b ? b : a;
  return p == (void*)0;
}
int maxne(char *a, char *b) {
  char *p = a < b ? b : a;
  return p == (void*)0;
}

int mineq(char *a, char *b) {
  char *p = a > b ? b : a;
  return p == (void*)0;
}
int minne(char *a, char *b) {
  char *p = a > b ? b : a;
  return p == (void*)0;
}
