/* { dg-options "-Woverloaded-virtual" } */

class Base {
public:
  virtual ~Base() {
  }
};

class Derived: public Base {
public:
  int Base() { // There should be no error here.
    return 5;
  }
};

int main() {
}
