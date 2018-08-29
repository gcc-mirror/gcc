// PR c++/84181
// { dg-do compile { target c++14 } }

template < int ... I >
struct A{};

template < typename T >
auto var = [](auto ... i){
        return A< decltype(i)::x ... >{};
    };

int main(){
    var< int >();
}
