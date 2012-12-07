// PR c++/54325
// { dg-options -std=c++11 }

class Base {
public:
  Base() {};
  virtual ~Base() {};

  virtual void do_stuff() = 0;
};

class Derived: public Base {
public:
  Derived() : Base{} {};
  virtual ~Derived() {};

  virtual void do_stuff() {};
};

int
main() {
  Derived d;

  return 0;
}
