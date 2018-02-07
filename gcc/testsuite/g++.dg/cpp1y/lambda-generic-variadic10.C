// PR c++/84182
// { dg-do compile { target c++14 } }

template < typename ... T > void sink(T ...){}

template < typename >
void f(){
    auto const lambda = [](int){ return 1; };

    [lambda](auto ... i){
        sink(lambda(i) ...);
    }(1);
}

int main(){
    f< int >();
}
