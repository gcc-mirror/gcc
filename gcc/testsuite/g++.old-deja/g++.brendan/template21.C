// Build don't link: 
// GROUPS passed templates
template<class T>
class L {
public:
    L();

    T x[30];
    int doit(int i) const;
};

#ifdef BUG
template<class T>
int
L<T>::doit(int i) const
{
    return x[i].z;
}
#endif

class X {
public:
    class Y {
    public:
        Y();
        Y(int);

        int z;
    };
    
    L<Y> ly;
};

#ifndef BUG
template<class T>
int
L<T>::doit(int i) const
{
    return x[i].z;
}
#endif

static X x;
