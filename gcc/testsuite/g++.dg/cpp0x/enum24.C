// PR c++/56749
// { dg-require-effective-target c++11 }

enum normal_enum
{
    not_scoped1,
    not_scoped2
};

enum class scoped_enum
{
    scoped1,
    scoped2
};

template <normal_enum N=not_scoped1>
class A
{
public:
    template <typename T>
        void fun ()
        {
        }
};

template <scoped_enum N=scoped_enum::scoped1>
class B
{
public:
    template <typename T>
        void fun ()
        {
        }
};


template <typename T>
void tfun ()
{
    A<> a;
    a.fun<char>(); //<------------ THIS IS FINE

    B<> b_defaulted;
    B<scoped_enum::scoped1> b_explicited;

    b_defaulted.fun<char>();          //<------------ UNEXPECTED: THIS FAILS
    b_defaulted.template fun<char>(); //<------------ THIS IS FINE

    b_explicited.fun<char>();         //<------------ UNEXPECTED: THIS FAILS
    b_explicited.template fun<char>();//<------------ THIS IS FINE
}

int main(int argc, char const *argv[])
{
    tfun<int>();
    return 0;
}
