// { dg-do assemble  }
// GROUPS passed templates
// PRMS Id: 13218

struct C {
        int x;
        char y;
        double z;
};
C c02;

template <int* ip> struct A {
        int* p;
        A() : p(ip) {}
};

template <C* cp> struct B {
        C* p;
        B() : p(cp) {}
};

int i00;

int main(void)
{
        A<&i00> a00;

        extern int i01;
        A<&i01> a01;

        B<&c02> b02;

        extern C c03;
        B<&c03> b03;
}

int i01;
C c03;
