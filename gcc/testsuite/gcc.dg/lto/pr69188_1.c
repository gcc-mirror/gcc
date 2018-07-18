/* PR ipa/69188 */
/* { dg-options "-flto -O1 -fprofile-generate" } */

extern void fn1(void);

int main() {
  fn1();
  return 0;
}

