// PR c++/123354
// { dg-do compile { target c++11 } }
// ICE with static local var referenced in NSDMI of local class

template<typename T>
void foo() {
    static constexpr int value = 42;
    struct s1_t {
        struct s2_t {
            int dummy { 0 };
            char* ptr { static_cast<char*>(::operator new(sizeof(value)))};
        } s2;
    } object;
}

int main() {
    foo<void>();
}
