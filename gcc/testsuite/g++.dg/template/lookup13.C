// PR c++/94799 - member template function lookup fails.

template <typename T>
struct A {
    int a() {
        return 42;
    }

    template<typename> struct X { typedef int type; };
};

template <typename T>
struct B {
    int b(A<T> *p) {
	int i = 0;
        i += p->a();
        i += p->template A<T>::a();
        i += p->template A<T>::template A<T>::a();
	i += A<T>().template A<T>::a();
	return i;
    }
};

int main() {
    A<int> a;
    B<int> b;
    return b.b(&a);
}
