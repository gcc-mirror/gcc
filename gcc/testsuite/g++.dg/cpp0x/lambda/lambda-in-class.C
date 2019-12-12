// { dg-do run { target c++11 } }

#include <cassert>

class C {
  private:
    int m_i;

  public:
    C() : m_i(-1) {
      //[] { this; } ();
      [this] () -> void { m_i = 0; } ();
      assert(m_i == 0);
      [this] () -> void { this->m_i = 1; } ();
      assert(m_i == 1);
      [&] () -> void { m_i = 2; } ();
      assert(m_i == 2);
      [&] () -> void { this->m_i = 3; } ();
      assert(m_i == 3);
      [=] () -> void { m_i = 4; } (); // copies 'this' or --copies-m_i--?
// { dg-warning "implicit capture" "" { target c++2a } .-1 }
      assert(m_i == 4);
      [=] () -> void { this->m_i = 5; } (); // { dg-warning "implicit capture" "" { target c++2a } }
      assert(m_i == 5);
    }

};

int main() {
  C c;

  //[this] () -> void {} (); // { dg-error: "cannot capture `this' outside of class method" }

  return 0;
}

