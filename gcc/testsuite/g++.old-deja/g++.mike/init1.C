// { dg-do run  }
int count;

extern "C" void _exit(int);

struct C {
  ~C() { if (count != 1) _exit(1); }
} c;

class A {
public:
  ~A () { ++count; }
};

void f() {
  static A a;
}

void g() {
  // Since this isn't constructed, we can't destruct it.
  static A a;
}

int main () {
  f();
}
