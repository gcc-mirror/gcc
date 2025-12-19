// PR c++/122550
// { dg-do compile { target c++20 } }

template<typename U>
struct Hasher;

template <class a>
concept C = true;

template<C T>
void add(Hasher<int>&, T);

template<>
struct Hasher<int> {
    template<C T>
    friend void add(Hasher& hasher, T integer) {}
};

int main() {
    Hasher<int> h;
    add(h, 0);
}
