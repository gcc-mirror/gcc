// Build don't link:

class X {
public:
  void x(void);
};

class Y : public X {
};

class Z : private Y {
public:
  void y(void);
};

void Z::y(void) {
  x();  // should be OK
}
