// PR c++/58932
// { dg-do compile { target c++11 } }

using nullptr_t = decltype(nullptr);

template<typename T, typename Sfinae = nullptr_t>
struct B {
    static float& int_if_addable();
};

template<typename T>
struct B<T, decltype( (T() + T()), nullptr )> {
    static int& int_if_addable();
};

struct X { };

struct Y { };
Y operator+(Y, Y);

struct Z { };
Z operator+(Z, Z) = delete;

int main()
{
  float& a = B<X>::int_if_addable();
  int& b = B<Y>::int_if_addable();
  float& c = B<Z>::int_if_addable();
}
