// The VxWorks kernel has no implementation of atexit, so local statics
// are never destroyed. 
// { dg-do run { xfail vxworks_kernel } }
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
