// PR c++/113108
// { dg-do compile { target c++17 } }

template <typename T>
struct Foo {
    Foo& operator=(Foo&&) = default;
    T data;
};

template <typename T>
void consume(Foo<T>& (Foo<T>::*)(Foo<T>&&) ) {}

template <typename T>
void consume(Foo<T>& (Foo<T>::*)(Foo<T>&&) noexcept) {}

int main() {
    consume(&Foo<int>::operator=);
}
