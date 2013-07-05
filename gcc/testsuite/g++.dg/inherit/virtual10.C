// PR c++/14263

struct A { };

struct B : virtual A { };

int main()
{
   A* p = new B;
   B* q = static_cast<B*>(p);  // { dg-error "cannot convert from pointer to base class 'A' to pointer to derived class 'B' because the base is virtual" }
}
