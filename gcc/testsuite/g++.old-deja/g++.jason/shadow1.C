// { dg-do assemble  }
// { dg-options "-Wshadow" }

class x {
public:
        void fun();
private:
        int foo;
};

void x::fun() { }

main ()
{
        float foo;
}
