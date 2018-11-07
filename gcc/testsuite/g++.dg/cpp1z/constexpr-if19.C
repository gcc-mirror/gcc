// PR c++/85200
// { dg-do compile { target c++17 } }

struct A{
    constexpr operator int(){ return 0; }
};

template < typename >
void f(){
    [](auto known){
        if constexpr(constexpr int k = known);
    }(A{});
}

int main(){
    f<int>();
}
