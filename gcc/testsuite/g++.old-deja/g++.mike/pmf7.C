// { dg-do run  }
class A;
typedef int (A::*f_ptr) (void);

class B {
public:
  B() {} ~B() {}
  B& dummy(f_ptr cb) { return *this; }
};

template<class SP, class CB> SP& call_dummy(SP* sp, CB cb) {
  sp->dummy(cb);
  return *sp;
}

class A {
public:
  A() {} ~A() {}
  int ok() { return 0; }
  A& call_it(B* s) {
    call_dummy(s, &A::ok);
    return *this;
  }
};

int main() {
  A a; B b;
  a.call_it(&b);
  return 0;
}
