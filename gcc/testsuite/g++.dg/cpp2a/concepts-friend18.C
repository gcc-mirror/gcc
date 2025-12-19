// PR c++/122550
// { dg-do compile { target c++20 } }

struct Hasher;
template <class a>
concept C = true;

template<C T>
void add(Hasher&, T);

struct Hasher {
    template<C T>
    friend void add(Hasher& hasher, T integer) {}
};

int main() {
    Hasher h;
    add(h, 0);
}
