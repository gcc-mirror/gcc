// Build don't link: 
class A {
char str[10];
public:
    char* m1 () { return str;};
};

class C : public A {
public:
};

class B : public A {
public:
    char* m1 () { C::m1(); return ""; } // ERROR - 
};

int main () {
A a;
B b;
C c;

a.m1();
c.m1();
b.m1();
}
