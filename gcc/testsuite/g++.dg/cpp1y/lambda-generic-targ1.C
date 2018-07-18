// PR c++/84181
// { dg-do compile { target c++14 } }

template < int ... I >
struct A{};

template < typename T >
void f(){
    [](auto ... i){
        return A< decltype(i){} ... >{};
    };
}

int main(){
    f< int >();
}
