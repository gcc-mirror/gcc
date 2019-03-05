struct S1
{
    int a, b, c;

    static immutable S1 C1 = { 1 2 3 }; // no commas here, compiles
    static immutable S1 C2 = { 1, 2, 3 }; // compiles as well
}
