// PR c++/28385
// instantiating op() with void()() was making the compiler think that 'fcn'
// was const, so it could eliminate the call.

// { dg-do run }

extern "C" void abort (void);

int barcnt = 0;

class Foo {
  public:
    template<typename T>
    void operator()(const T& fcn) {
      fcn();
    }
};

void bar() {
  barcnt++;
}

int main() {
  Foo myFoo;
  myFoo(bar);
  myFoo(&bar);
  if (barcnt != 2)
    abort ();
  return 0;
}
