/* { dg-do compile } */
/* { dg-options "-gbtf -gtoggle" } */

struct A {
  int x;
  int y;
  char c;
};

int
foo (struct A *a) {
  int y = __builtin_preserve_access_index (a->y); /* { dg-error "BPF CO-RE is required" } */
  return y;
}
