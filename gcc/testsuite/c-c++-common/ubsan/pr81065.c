/* { dg-do run } */
/* { dg-options "-fsanitize=undefined -fsanitize-undefined-trap-on-error" } */

unsigned char x = 154;
int foo() {
  // 8575 * (254408 - 9057) = 8575 * 245351 = 2103884825 = 0x7d66bc19
  return 8575 * (1652 * x - 9057);
}

int main() {
  foo();
  return 0;
}
