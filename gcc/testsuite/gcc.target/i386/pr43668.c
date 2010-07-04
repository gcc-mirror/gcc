/* PR target/43668 */
/* { dg-do run } */
/* { dg-options "-fschedule-insns" } */

int foo(int i, ...) {
  return i;
}
int main() {
  return foo(0, 0.0);
}
