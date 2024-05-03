// { dg-do compile { target c++11 } }
// { dg-options "-O2 -fno-ivopts -fdump-tree-optimized" }

template <class T>
struct simple_optional {
    bool has_val;
    T val;

    auto begin() const -> T const* { return &val; }
    auto end() const -> T const* { return &val + (has_val ? 1 : 0); }
};

void f(int);

void call_f(simple_optional<int> const& o) {
    for (int i : o) {
        f(i);
    }
}

// Only a conditional execution of 'f' should prevail, no loop
// { dg-final { scan-tree-dump-times "<bb" 5 "optimized" } }
