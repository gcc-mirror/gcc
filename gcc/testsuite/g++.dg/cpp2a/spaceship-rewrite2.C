// Test that C++20 overload changes don't break sloppy code.

struct C {
    bool operator==(const C&);
    bool operator!=(const C&);
};

int main() {
    C c1, c2;
    (void)(c1 == c2);
    (void)(c1 != c2);
}
