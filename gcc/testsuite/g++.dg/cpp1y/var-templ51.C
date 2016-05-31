// PR c++/60095
// { dg-do link { target c++14 } }

template <class>
constexpr bool b = false;
template<typename T>
constexpr bool b<T*> = true;
int main() {
    b<int*>;
    b<double*>;
}
