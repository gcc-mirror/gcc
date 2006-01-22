// { dg-do compile }
// { dg-options "-fno-rtti" }

// PR C++/10891

struct A {
   virtual ~A() { }
};

struct B : A {
};

A* f();

int main()
{
   B* b = dynamic_cast<B*>(f()); // { dg-error "error: " }
}
