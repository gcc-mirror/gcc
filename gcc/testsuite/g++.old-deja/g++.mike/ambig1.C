// { dg-do assemble  }
extern "C" int printf(const char *, ...);

struct VB {
  virtual void f() {
    printf("VB\n");
  }
};

class M : public virtual VB {
public:
  int i;
  void f() {
    printf("M(%d)\n", i);
  }
};

class lM : public M {
};

class rM : public M {
};

class D : public lM, rM { // { dg-error "" } ambiguous function
} d;

int main() {
  ((lM*)&d)->i = 1;
  ((rM*)&d)->i = 2;
  ((rM*)&d)->f();
  ((lM*)&d)->f();
  ((VB*)&d)->f();
}
