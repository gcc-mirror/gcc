// { dg-do link  }
// GROUPS passed templates membertemplates
template<class T, int N>
class Foo {

public:
    template<int N2>
    Foo<T,N> operator=(const Foo<T,N2>& z)
    {
        return Foo<T,N>();
    }
};

int main()
{
    Foo<double,4> x;
    Foo<double,7> y;
    x = y;

    return 0;
}

