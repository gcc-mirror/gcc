void foo(signed char) {}
typedef int bar;
void foo(bar) {}

int main (int, char **) {
  char c;
  foo(c);
  return 0;
}
