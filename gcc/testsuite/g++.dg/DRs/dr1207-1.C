// DR 1207
// PR c++/52869
// { dg-do compile { target c++11 } }

struct S {
    void f() { }
    void g() noexcept(noexcept(f())) { }
    void h() noexcept(noexcept(this->f())) { }
};

struct Nyan {
	Nyan &operator++() noexcept { return *this; }
	void omg() noexcept(noexcept(++*this)) {}
};

template <class T>
class Test{
    T count;
    Test (T arg) {count=arg;}
    void fetch() { }
    T inc () noexcept(noexcept(this->fetch())) {return ++count;}
    T dec () noexcept(noexcept(fetch())) { return --count;} 
};
