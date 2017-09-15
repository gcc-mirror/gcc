// PR c++/81180
// { dg-options -std=c++17 }

template < int I > struct int_{};

template < typename T >
struct A{
    template < typename U, int I >
    struct B{
        B(U u, int_< I >){}
    };
};


int main(){
    A< int >::B v(0, int_< 0 >());
    (void)v;
}

