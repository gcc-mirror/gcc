// Special g++ Options: -Wshadow
// Build don't link:

class x {
public:
        void fun();
private:
        int foo;
};

void x::fun() { };

main ()
{
        float foo;
}
