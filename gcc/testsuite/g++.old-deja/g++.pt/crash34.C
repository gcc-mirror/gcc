// Build don't link:
// Origin: Theodore Papadopoulo <Theodore.Papadopoulo@sophia.inria.fr>

template <class T>
class A {
public:
        class B { };
        class C: public B {
        public:
                C(A&):B() { }
        };
        C f() { return C(*this); }
};

int
main()
{
        A<int> a;
        a.f();
}
