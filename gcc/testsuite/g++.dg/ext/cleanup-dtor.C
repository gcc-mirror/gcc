// Check that destructors are run after cleanup functions.
// { dg-do run }

extern "C" void abort ();

int i;

struct S {
  ~S() {
    if (i != 1)
      abort ();
    i = 2;
  }
};

void f(void *) {
  if (i != 0)
    abort ();
  i = 1;
}

int main () {
  {
    S s __attribute__((cleanup (f)));
  }
  if (i != 2)
    abort ();
}
