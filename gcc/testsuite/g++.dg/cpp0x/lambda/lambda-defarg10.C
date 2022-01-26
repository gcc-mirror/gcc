// PR c++/103186
// { dg-do compile { target c++11 } }

struct f
{
  template<class T1>
   f(const T1&){}
};


template<typename T> class A {
public:
    void foo(A<T> a, const f& fn = [](){}) { }
    void bar(A<T> a) { foo(a); }
};
int main() {
    A<int> a;
    a.foo(a);
    a.bar(a);
    return 0;
}
