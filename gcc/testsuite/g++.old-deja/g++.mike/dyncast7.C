// Special g++ Options: -fexceptions

#include <typeinfo>
#include <stdexcept>

class A {
public:
  virtual void j () {}
};

class B : public A { };
     
void x (A& a) {
  // These should all work.
  const B& b2 = dynamic_cast<B&>(a);
  const B& b3 = dynamic_cast<const B&>((const A&)a);
  const B& b4 = dynamic_cast<const B&>(a);
}

int main() {
  try {
    B b;
    x (b);
  } catch (std::exception& e) {
    // If we get a bad_cast, it is wrong.
    return 1;
  }
}
