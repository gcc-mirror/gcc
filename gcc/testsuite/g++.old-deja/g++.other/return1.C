// Build don't link:
// Special g++ Option: 
// Origin: holmen@mail.nu

struct C {
    int f() {return 0;}
};

struct D {
    C a[1];
    C* g();
};

C* D::g() {
    int i = 0;
    while (i < 1 && a[i].f() != 1) {}
    return undefined_variable; // ERROR - 
}
