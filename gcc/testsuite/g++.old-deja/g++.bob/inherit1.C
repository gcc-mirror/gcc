// { dg-do assemble  }
class A {
char str[10];
public:
    char* m1 () { return str;}
};

class C : public A {
public:
};

class B : public A {
public:
    char* m1 () { C::m1(); return ""; } // { dg-error "cannot call" } 
    // { dg-warning "deprecated|forbids converting a string constant" "depr" { target *-*-* } .-1 }
};

int main () {
A a;
B b;
C c;

a.m1();
c.m1();
b.m1();
}
