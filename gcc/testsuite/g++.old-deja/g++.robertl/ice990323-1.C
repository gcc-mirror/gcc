// Build don't link:

//test 2
struct A {};
void f()
{
        struct A; // ERROR - forward ref
        throw *(new A); // ERROR - invalid use of undefined type
}
