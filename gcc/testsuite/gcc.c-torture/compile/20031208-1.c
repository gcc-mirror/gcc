extern int foo(int, ...);
int bar(void) {
  long double l = 1.2345E6;
  foo(0, l);
  return 0;
}
