// Build don't run:
// Special g++ Options: -O3
// Origin: Gabriel Dos_Reis <Gabriel.Dos_Reis@sophia.inria.fr>

void f() {}

struct X {
    ~X() { f (); }
};

X x;

int main () {}
