// Build don't link:

// make sure extern "C" friends work.

extern "C" { void func(); }

struct crash {
  int i;
  friend void func();
} a;

void func() {
  a.i = 1;
}
