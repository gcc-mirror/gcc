#include <iostream.h>

class A1 {
        friend class B;
 public:
        virtual void foo() {};
};

class A2 : public virtual A1 {friend class B;};

class A3 : public virtual A1, private A2 {friend class B;};

class B
{
 public:
        B(A1* a) : itsA(dynamic_cast<A2*>(a)) {};
        A2* itsA;
};

int main()
{
        A1* a=new A3;
        B b(a);

        if (b.itsA) cout << "cast ok" << endl; else cout << "cast failed" << endl;
        return 0;
}
