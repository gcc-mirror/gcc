// PRMS Id: 2139
// Bug: g++ tries to instantiate the template with types on the function
// obstack and fails.

template<class T>
class X {
public:
    X(int) { }

    T x;
};

class A { };

int main()
{
    int i;
    X<int> xi(i);
    X<double> xd(i);
    
    X<int (*)(int, void *)> fp0(i);
    X<int (*)(int, char, double)> fp1(i);
    X<int (*)(int, double**, void *)> fp2(i);

    X<int (A::*)()> mp0 (i);
    X<int A::*> mp1 (i);
}
