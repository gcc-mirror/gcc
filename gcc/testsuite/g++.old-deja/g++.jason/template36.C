// Testcase for implicit 'typename' and resolution of 'typename's in the
// current scope.

class base1 { 
public:
    int bar() const
    { return 1; }
};

class base2 { 
public:
    int bar() const
    { return 0; }
};

template<class X>
struct base_trait {
    typedef base1 base;
};

template<>
struct base_trait<float> {
    typedef base2 base;
};

template<class T>
class weird : public base_trait<T>::base {
public:
    typedef typename base_trait<T>::base base;

    base f ();
    int base::* g ();

    int zowee() const
    { return bar(); }  
};

template <class T>
typename weird<T>::base weird<T>::f ()
{
    return base();
}

// The standard does not allow this case; the `typename' keyword may
// not appear in a ptr-operator.
#if 0
template <class T>
int typename weird<T>::base::* weird<T>::g ()
{ return 0; }
#endif

int main()
{
    weird<float> z;
    return z.zowee() || z.f().bar();
}
