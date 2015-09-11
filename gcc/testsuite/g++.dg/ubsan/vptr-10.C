// { dg-do run }
// { dg-options "-fsanitize=vptr -fno-sanitize-recover=vptr" }

struct A
{
    virtual ~A() {}
};
struct B : virtual A {};
struct C : virtual A {};
struct D : B, virtual C {};

int main()
{
    D d;
}
