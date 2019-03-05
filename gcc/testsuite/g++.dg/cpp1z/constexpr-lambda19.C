// PR c++/84098
// { dg-do compile { target c++17 } }

struct A{};

template < typename >
struct Test{
    static constexpr auto var = []{};
};

int main(){
    (void)Test< A >::var;
}
