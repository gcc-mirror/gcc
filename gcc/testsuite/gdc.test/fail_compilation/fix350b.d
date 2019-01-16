int foo() { return 3; }

struct S2
{
    int a, b, c;

    static immutable S2 C1 = { foo() 2 3 }; // compiles (and works)
    static immutable S2 C2 = { foo() 2, 3 }; // compiles (and works)
    //static immutable S2 C3 = { 2 foo() 3 }; // does not compile: comma expected separating field initializers
}
