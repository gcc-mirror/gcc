// prms-id: 9206

class X {
public:
  void xtest() { }
};

class Y { };

typedef void (X::*Xptr)();
typedef void (Y::*Yptr)();

int main() {
  X xx;

  Xptr xp = &X::xtest;
  Yptr yp = reinterpret_cast<Yptr>(xp);
  xp = reinterpret_cast<Xptr>(yp);

  (xx.*xp)();
}
