// Build don't link:

class c {
  void (c::*x)();
public:
  void f() { this->x(); } // ERROR - pointer-to-member
};
