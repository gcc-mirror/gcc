// Copyright 2002 Free Software Foundation

// Derived by Alexandre Oliva <aoliva@redhat.com> from code posted by
// Mark Mitchell <mark@codesourcery.com>

typedef struct {
  void f();
} S;

int main() {
  S s;
  s.f();
}
