// { dg-do compile }

// Origin: Jiangbin Zhao <zhaojiangbin@yahoo.com>

// PR c++/12369: ICE for specialization of member function template
// as friend in ordinary class.

struct A {
    template<class T> T* make() { return new T(); }
};
 
struct B {
    friend B* A::make< B >(); // (1)
};
