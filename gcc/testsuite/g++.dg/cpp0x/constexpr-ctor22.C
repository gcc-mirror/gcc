// PR c++/116722
// We're now accepting this in spite of the virtual base class. This is OK
// according to [dcl.constexpr] 3: "Except for instantiated constexpr functions
// non-templated constexpr functions shall be constexpr-suitable".
// { dg-do compile { target c++11 } }

class base {};
class derived : virtual public base {
public:
  template<typename Arg>
  constexpr derived(Arg) {}
};
int main() {
  derived obj(1.);
}
