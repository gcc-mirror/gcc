struct S {
  int i;
};

void f() {
  int const S::*p;
  const_cast<int const S::*>(p);
}

