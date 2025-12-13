// { dg-do compile }
// PR c++/122509

namespace s {
  template<class A>
  struct v {
    void size() {}
  };
}
struct D : public s::v<double>, public s::v<int> {};
int main() {
  D().v<int>::size();
}
