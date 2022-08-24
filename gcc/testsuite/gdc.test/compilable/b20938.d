// issue 20938 - Cannot create const arrays mixing immutable and mutable structs with indirections
struct S { int[] a; }
enum A { a }
enum B { b }

void fun() {
    int* pi;
    immutable int* ipi;
    int[] ai;
    immutable int[] iai;
    S s;
    immutable S _is;
    Object o;
    immutable Object io;

    auto a = [pi, ipi];
    auto b = [ai, iai];
    auto c = [s, _is];
    auto d = [o, io];

    auto e = [A.a, B.b];
}
