// PR c++/66878

struct S;

namespace H {
    namespace P {
        using ::S;
    }
    struct P::S {};
}

int main() {}
