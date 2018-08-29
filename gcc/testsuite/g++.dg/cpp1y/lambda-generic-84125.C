// PR c++/84125
// { dg-do compile { target c++14 } }

struct X { constexpr operator bool() const { return true; } };

int main(){
    [](auto) {
        static_assert(X{}, "");
    };
}
