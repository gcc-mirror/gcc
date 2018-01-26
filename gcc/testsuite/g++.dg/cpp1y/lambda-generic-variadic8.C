// PR c++/84036
// { dg-do compile { target c++14 } }

template < typename T >
auto f(T){
    [](auto ... i){
        [i ...]{};
    };
}

int main(){
    f(0);
}
