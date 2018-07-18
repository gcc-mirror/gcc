// PR c++/80767
// { dg-options -std=c++17 }

template <typename... Fs> 
struct overloader : Fs...
{
    overloader(Fs... fs) 
        : Fs(fs)...
    { } 

    using Fs::operator()...;
};

struct a { void foo() { } };
struct b { void bar() { } };
struct c { void bar() { } };

int main() {
    overloader{
        [](a x) { x.foo(); },
        [](auto x) { x.bar(); }
    }(a{});
}
