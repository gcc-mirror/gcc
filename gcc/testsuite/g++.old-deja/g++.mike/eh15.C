// Build don't link:
// Special g++ Options: -fexceptions

struct A {
  A() throw (int);
};
