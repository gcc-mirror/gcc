// PR c++/96673
// { dg-do compile { target c++11 } }

template <class T>
class A {};

template <class T>
class B;

template <class T>
class C {
    private:

    friend class B<T>;

    explicit C(A<T>&) {};
};


template <class T>
class B {
    public:
    B() = default;
    //B() {};       // << This implementation of the constructor makes it work

    A<T> a = {};
    C<T> c = C<T>{a};
};

int main() {
    auto b = B<int>{};
    auto &c = b.c;
}
