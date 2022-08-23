// PR c++/104642

// At -O0 and -Og we default to -funreachable-traps
// so the below should abort at runtime.

// { dg-do run }
// { dg-shouldfail { *-*-* } }
// { dg-additional-options "-O2" }

bool b;

__attribute__ ((optimize ("Og")))
int f() {
  if (b) return 42;
}			// { dg-warning "-Wreturn-type" }

int main() { f(); }
