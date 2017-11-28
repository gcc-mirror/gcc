// PR c++/66690
// { dg-do compile { target c++14 } }

template<class T> auto foo(T t) {return 3;}
class B {B();};  // { dg-message "declared private" }
template<class T> class D:public B
{
    D()  // { dg-message "declared private" }
    {  // { dg-error "is private" }
        T x00;
        foo(x00);
    }
};
D<int> d;  // { dg-error "is private" }
