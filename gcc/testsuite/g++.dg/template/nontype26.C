// PR c++/31671

template<int& i> void doit() {
  i = 0;
}

template<const int& i> class X {
public:
    void foo() {
      doit<i>();  // { dg-error "cv-qualification|no matching" }
    }
};

int i = 0;

X<i> x;

int main() {
  x.foo();
}
