// { dg-do link  }
// { dg-options "-O3" }
// Origin: Gabriel Dos_Reis <Gabriel.Dos_Reis@sophia.inria.fr>

void f() {}

struct X {
    ~X() { f (); }
};

X x;

int main () {}
