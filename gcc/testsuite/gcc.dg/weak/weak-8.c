/* { dg-do assemble } */

__attribute__ ((weak)) int i;

int f() {
  return i;
}
