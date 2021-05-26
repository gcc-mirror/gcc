// PR c++/97420
// { dg-do compile { target c++11 } }

int f(int) noexcept;
template<int (&)(int)> void A();
int main() {
  A<f>();
}
