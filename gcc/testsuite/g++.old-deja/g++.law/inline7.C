// { dg-do assemble  }
// GROUPS passed inlining
   template <class Type>
struct A {
   typedef int X;
   A() {}
   virtual ~A() { }
};
   template <class Type>
struct B : public A<Type> {
   B() { }
}; 
B<int>::X x;
