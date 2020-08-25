/* { dg-do compile } */
/* { dg-options "-O1" } */

struct a {
  int b;
  int c;
} d() {
  struct a e[9];
  int f = 3362953455;
  e[f] = e[6];
  e[6].c = 1;
}
int main() {}
