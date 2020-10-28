/* { dg-do compile } */
/* { dg-options "-O2" } */

struct a {
  unsigned c : 17;
};
struct a b;
int d(void) {
  short e = b.c;
  return e ? 0 : b.c;
}
