/* -gbtf for the BPF target should enable CO-RE support automatically.  */
/* { dg-do compile } */
/* { dg-options "-gbtf" } */

struct A {
  int x;
  int y;
  char c;
};

int
foo (struct A *a) {
  int y = __builtin_preserve_access_index (a->y);
  return y;
}
