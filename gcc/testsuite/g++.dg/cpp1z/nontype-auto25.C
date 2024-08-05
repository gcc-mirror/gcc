// PR c++/116223
// { dg-do compile { target c++17 } }

template <int T> struct A { int value = T; };

template <unsigned char X> using B = A<X>;

template <auto X>
void foo(B<X>& mat) noexcept
{
  //    std::cout << mat.value << "\n";
}

int main()
{
    A<2> mat;
    foo(mat);
}
