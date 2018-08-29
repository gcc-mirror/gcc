// PR sanitizer/78651
// { dg-do run }
// { dg-additional-options "-fpic" { target fpic } }

struct A { };

namespace {

void thisThrows () {
  throw A();
}

struct SomeRandomType {};
}

int main() {
  try {
    thisThrows();
  }
  catch (SomeRandomType) {
    throw;
  }
  catch (A) {
  }
  return 0;
}
