// PR c++/117099
// { dg-do "compile" }

struct X {
  ~X();
};

X test(bool b) {
  {
    X x;
    return x;
  }
  return X();
  if (!(b)) return; // { dg-error "return-statement with no value" }
}
