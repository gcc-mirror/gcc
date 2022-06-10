// PR c++/104642

// At -O0 and -Og we default to -funreachable-traps
// so the below should abort at runtime.

// { dg-do run }
// { dg-shouldfail { *-*-* } }
// { dg-additional-options "-O0" }

bool b;

int f() {
  if (b) return 42;
}			// { dg-warning "-Wreturn-type" }

int main() { f(); }
