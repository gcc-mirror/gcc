// prms-id: 7865

int count;

struct A {
  A() { ++count; }
  ~A() { --count; }
};

int foo() { return 1; }

int bar()
{
  A a;
  for (;;) {
    A b;
    if (foo())
      return 0;
    if (foo())
      return 0;
  }
  return 0;
}

int main() {
  bar();
  return count;
}
