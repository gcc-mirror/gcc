// PR c++/110584
// { dg-do "run" { target c++11 } }

void foo (int i) {
  if (i != 0)
    __builtin_abort ();
}

int main () {
  const int x = 0;

  // We would error out on this.
  (void) [&] () {
    foo (x);
    (void) [] () {
      foo (x);
    };
  } ();
  // As well as those.
  (void) [&] () {
    (void) [] () {
      foo (x);
    };
  } ();
  (void) [&x] () {
    (void) [] () {
      foo (x);
    };
  } ();
  // But those would work already.
  (void) [] () {
    (void) [&] () {
      foo (x);
    };
  } ();
  (void) [&] () {
    (void) [&] () {
      foo (x);
    };
  } ();
  (void) [=] () {
    (void) [] () {
      foo (x);
    };
  } ();
}
