// { dg-do run  }
// { dg-options "-O" }
// Origin: Andrew Pollard <andrew@odie.demon.co.uk>

struct A {
        A(int, int);
};
A::A(int, int) {}
static A _A(0, 0);
int main() { return(0); }
