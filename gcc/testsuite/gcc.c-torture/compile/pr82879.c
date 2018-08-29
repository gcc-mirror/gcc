int a, b;
static __attribute__((cold)) void fn1() {
  for (;;)
    for (; a;)
      ;
}
void fn2() {
  if (b)
    fn1();
}

