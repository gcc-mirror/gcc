// PR c++/59816
// { dg-do compile { target c++11 } }

class Base {
protected:
  template<class... TArgs>
  Base(TArgs...) {}
};

class Class
  : public Base {
public:
  template<class... TArgs>
  Class(TArgs... args) : Base { args... } {}
};

void test() {
  Class{};
}
