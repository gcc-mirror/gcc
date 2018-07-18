// PR c++/84098
// { dg-options -std=c++17 }

struct A{};

template < typename >
struct Test{
    static constexpr auto var = []{};
};

int main(){
    (void)Test< A >::var;
}
