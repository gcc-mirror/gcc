// simplified from testcase in Windows Developer Journal,
// submitted by eyal.ben-david@aks.com

// The initialization of a static local variable must be retried if a
// previous try finished by throwing an exception [stmt.dcl]/4

extern "C" void abort ();

struct foo {
  foo() { throw true; }
};

void bar() {
  static foo baz;
}

int main() {
  try {
    bar(); // must throw
  }
  catch (bool) {
    try {
      bar(); // must throw again!
    }
    catch (bool) {
      return 0;
    }
  }
  abort();
}
