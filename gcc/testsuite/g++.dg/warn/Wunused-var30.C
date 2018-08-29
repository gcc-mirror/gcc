// PR c++/83942
// { dg-do compile { target c++11 } }
// { dg-additional-options "-Wall" }

enum class E { E1 };
int main() {
    E const e = E::E1;
    return static_cast<int>(e);
}
