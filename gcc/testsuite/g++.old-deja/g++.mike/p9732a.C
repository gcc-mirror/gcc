// { dg-do run  }
// prms-id: 9732

class A {
  int i;
public:
  A() { i = 1; }
  ~A() { }
};

struct value {
  A x,y,z;
};

int crash(const value* capt, value* jet) {
  return capt == jet;
}

class euler {
public:
  value number() const { return _value; }
  int distance();

private:
  value _value;
};

int euler::distance() {
  const value& capt = number();
  value jet;
  return crash (&capt, &jet);
}

int main() {
  euler e;
  return e.distance();
}
