// PR c++/10841

int main() {
  class Base { 
  public: 
    int i, j, k; 
    void f(); };

  class Derived : private Base { 
  public: 
    int m, n, p; 
    void g(); 
  };

  Derived derived;
  Base &base = (Base &)derived;
  (int Base::*)&Derived::n;
  (int Derived::*)&Base::j;
  (void (Base::*)(void))&Derived::g;
  (void (Derived::*)(void))&Base::f;
}

