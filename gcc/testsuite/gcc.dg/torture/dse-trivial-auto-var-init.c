/* Testcase for LLVM bug: https://github.com/llvm/llvm-project/issues/119646 */
/* { dg-do run } */
/* { dg-additional-options "-ftrivial-auto-var-init=zero" } */

int b = 208;
[[gnu::noinline]]
void f(int *e, int a) {
  *e = !!b;
  if (a)
    __builtin_trap();
}
int main(void) {
  b = 0;
  f(&b, 0);
  if (b != 0)
    __builtin_trap();
}
