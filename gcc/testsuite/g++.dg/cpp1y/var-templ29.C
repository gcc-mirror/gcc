// PR c++/65719
// { dg-do link { target c++14 } }

struct FunctionObject {
    void operator()() const { }
};

template <typename T>
constexpr FunctionObject f{};

int main() {
    f<int>();
}
