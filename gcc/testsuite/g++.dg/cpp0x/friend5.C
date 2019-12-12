// PR c++/61327
// { dg-do compile { target c++11 } }

class B {
protected:
  void f() {}
};

template <typename...>
struct S;

template <typename R>
struct S<R>{
    template <typename T>
    static void caller(T *p) {p->B::f();}
};

class Q : B{
template <typename...> friend struct S;
};

int main(){
    Q q;
    S<int>::caller(&q);
    return 0;
}
