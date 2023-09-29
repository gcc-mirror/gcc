/* { dg-do compile } */
/* { dg-options "-O3 -Wno-div-by-zero" } */

/* Make sure no ICE. */
int main() {
  unsigned b;
  return b ? 1 << --b / 0 : 0;
}
