template <class Derived>
struct base {
  friend void bar(Derived& d) {
    d.bar(); // access in inline friend of friend, ok?
  }
};

class derived : base<derived> {
  friend class base<derived>;
  void bar() {}
};

int main() {
  derived d;
  bar(d);
}

