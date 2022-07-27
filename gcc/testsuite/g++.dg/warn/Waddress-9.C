// PR c++/105569
// { dg-do compile { target c++11 } }
// { dg-options -Waddress }

class A {};

class B : public virtual A {};

class C : public A {};

int main() {
    B* object = new B();
    B &ref = *object;

    bool b = nullptr == dynamic_cast<A*>(&ref); // { dg-warning "the address of 'ref' will never be NULL" }
    bool b4 = nullptr == static_cast<A*>(&ref); // { dg-warning "the address of 'ref' will never be NULL" }
    if (dynamic_cast<A*>(&ref)) // { dg-warning "the address of 'ref' will never be NULL" }
      {
      }
    if (static_cast<A*>(&ref)) // { dg-warning "the address of 'ref' will never be NULL" }
      {
      }

    auto ptr = dynamic_cast<A*>(&ref);
    bool b2 = ptr == nullptr;

    C* cobject = new C();
    C &cref = *cobject;

    bool b3 = nullptr == dynamic_cast<A*>(&cref);
}
