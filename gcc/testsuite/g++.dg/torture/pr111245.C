/* { dg-do run } */

struct Int {
  int value;
};

__attribute__((noipa)) Int always_throws() { throw 123; }

void foo(Int &x) {
  try {
    x = always_throws();
  } catch (...) {
  }
}

int main()
{
  Int x;
  x.value = 5;
  foo(x);
  if (x.value != 5)
    __builtin_abort ();
}
