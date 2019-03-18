// PR c++/85646
// { dg-do compile { target c++11 } }
// { dg-require-visibility "" }
// { dg-additional-options -fvisibility=hidden }

template<typename T>
void foo() {
    struct inner {
        inner() {
            (void)([this] { });
        }
    };
}

int main() { foo<int>(); }
