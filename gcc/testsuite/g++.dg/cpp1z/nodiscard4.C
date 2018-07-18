// PR c++/79502
// { dg-do compile { target c++11 } }

template<typename>
struct [[nodiscard]] missiles {};

missiles<void> make() { return {}; }
missiles<void> (*fnptr)() = make;

int main()
{
  make();	// { dg-warning "ignoring returned value of type" }
  fnptr();	// { dg-warning "ignoring returned value of type" }
}
