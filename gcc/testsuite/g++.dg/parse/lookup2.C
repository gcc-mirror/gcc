template <typename T> struct A
{
   typedef int X;
};

template <typename T> struct B
{
   typename A<T>::X x;
};

template <typename T> struct C
{
   void foo(int);
   B<A<T>*> b;
};

template <typename T> struct D
{
   enum { e };
   void bar() { C<T*>::foo(e); }
};
