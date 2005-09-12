// Test that we look up a friend declared at top level ahead of an
// undeclared friend found by argument dependent lookup.

// { dg-do run }

int f(int) { return 0; }

struct S {
  friend int f(char) { return 1; }
};

int main () { return f('a'); }
