// Error: Internal Compiler error on GCC 2.7.2.3 & EGCS 1998/05/23 snapshot.

class A {
public:
        enum { ONE, TWO, THREE };
};

template <const unsigned c1,const unsigned c2,const unsigned c3>
void f() {

}

int
main()
{
        f<A::ONE,A::TWO,A::THREE>();
}
