// PR c++/84338
// { dg-do compile { target c++14 } }

template < typename ... T >
auto f(T ... i){
    [](auto ... i){
        // // wrongly true in current trunk
        // static_assert(sizeof...(i) == 1, "");
        static_assert(sizeof...(i) == 2, "");
    }(i ...);
}

int main(){
    f(0, 1);
}
