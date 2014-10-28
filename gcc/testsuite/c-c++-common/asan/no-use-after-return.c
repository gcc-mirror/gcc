/* { dg-do compile } */
/* { dg-options "--param asan-use-after-return=0" } */

extern void f(char *);

int main() {
  char buf[64];
  f(buf);
  return 0;
}

/* { dg-final { scan-assembler-not "__asan_option_detect_stack_use_after_return" } } */
