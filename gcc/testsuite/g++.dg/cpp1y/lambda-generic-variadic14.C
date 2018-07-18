// PR c++/84368
// { dg-do compile { target c++14 } }

template < typename ... T >
void sink(T ...){}

template < typename ... T >
void foo(T ... v){
    [](auto ... v){
        auto bar = [](auto, auto){ return 0; };
        sink(bar(v, T{}) ...);
    }(v ...);
}

int main(){
    foo(0);
}
