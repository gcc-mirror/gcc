// Bug: g++ tries to look inside (B*)0 for a virtual base pointer.

struct A
{
};

struct B : virtual A
{
};

A* a;

int main()
{
    a = (B*)0;
}
