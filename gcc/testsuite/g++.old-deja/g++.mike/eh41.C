// { dg-do run { xfail sparc64-*-elf arm-*-pe } }
// { dg-options "-fexceptions" }

int fail = 0;

struct A {
   A () { a = 'a'; b = 'b'; c = 'c'; }
   ~ A () {
      if ( a != 'a' ) fail = 1;
      if ( b != 'b' ) fail = 1;
      if ( c != 'c' ) fail = 1;
   }
   char a, b, c;
};

void some_init () { throw 1; }

struct C : A {
   C () { some_init (); }
};

int main () {
  try {
    C c;
  } catch (int i) {
    return 0;
  }
  return 1;
}
