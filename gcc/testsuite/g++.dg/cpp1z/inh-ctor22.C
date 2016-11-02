// Testcase from P0136
// { dg-do compile { target c++11 } }

struct B1 {
  template <class... Ts>
  B1(int, Ts...) { }
};

struct B2 {
  B2(double) { }
};

int get();

struct D1 : B1 {		// { dg-message "B1::B1" }
  using B1::B1;  // inherits B1(int, ...)
  int x;
  int y = get();
};

void test() {
  D1 d(2, 3, 4); // OK: B1 is initialized by calling B1(2, 3, 4),
  // then d.x is default-initialized (no initialization is performed),
  // then d.y is initialized by calling get()
  D1 e;          // { dg-error "" } D1 has a deleted default constructor
}

struct D2 : B2 {
  using B2::B2;			// { dg-message "B1::B1" }
  B1 b;
};

D2 f(1.0);       // { dg-error "" } B1 has no default constructor
