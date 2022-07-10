// PR c++/104642

// With -fsanitize=unreachable we shouldn't optimize away the call to f.

// { dg-do run }
// { dg-shouldfail { *-*-* } }
// { dg-additional-options "-O -fsanitize=unreachable" }

bool b;

int f() {
  if (b) return 42;
  __builtin_unreachable ();
  return 24;
}

int main() { f(); }
