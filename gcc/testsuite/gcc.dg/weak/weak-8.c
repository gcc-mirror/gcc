/* { dg-do assemble } */
/* { dg-require-weak "" } */

__attribute__ ((weak)) int i;

int f() {
  return i;
}
