// prms-id: 5469

int count;

class A {
  A();
  A(const A&);			// ERROR - referenced below
public:
  A(int) { ++count; }
  ~A() { --count; }
  int operator== (const A& r) { return 0; }
};

int main() {
  {
    A a (1);
    if (a == 2 && a == 1)	// ERROR - private copy ctor
      ;
  }
  return count;
}
