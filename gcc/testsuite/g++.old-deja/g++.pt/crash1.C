template<class T> class A {
    public:
        class subA {};
};


template<class T> class B : public A<T> {
    public:
  class subB : public A::subA {}; // ERROR - not a class or namespace
};
 
