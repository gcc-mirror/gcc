// PR c++/90782
// { dg-do compile { target c++17 } }

template<class... A>
struct bar {
    template<class B>
    bar(B& obj, void(B::*f)(A...)const=&B::operator()){}
};
int main() {
    const auto f1 = [](){};
    bar f8(f1);
}

