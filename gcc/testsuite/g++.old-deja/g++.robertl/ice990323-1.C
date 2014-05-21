// { dg-do assemble  }

//test 2
struct A {};
void f()
{
        struct A; // { dg-message "" } forward ref
        throw *(new A); // { dg-error "" } invalid use of undefined type
}
