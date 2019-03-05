/*
TEST_OUTPUT:
---
fail_compilation/ice13024.d(15): Error: cannot implicitly convert expression `t.x` of type `A` to `B`
---
*/

enum A { a }
enum B { b }
struct T { A x; B y; }
void main()
{
    T t;
    auto r1 = [cast(int)(t.x), cast(int)(t.y)]; // OK
    auto r3 = [t.x, t.y]; // crash
}
