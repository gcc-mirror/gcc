// PR102801
// { dg-do compile }
// { dg-require-effective-target c++17 }
// { dg-options "-O2 -Wall" }
// { dg-skip-if "requires hosted libstdc++ for string" { ! hostedlib } }

#include <algorithm>
#include <memory>
#include <optional>
#include <string>
#include <utility>
#include <vector>

class C {
    bool b{}; // { dg-bogus "uninitialized" }

    struct Shared {};
    using SharedPtr = std::shared_ptr<const Shared>;

    SharedPtr shared;

public:
    C() = delete;
    C(bool bIn) : b(bIn) {}
    ~C();
    int someMethod() const;
};

using OptC = std::optional<C>;

class C2 {
    OptC c;
public:
    C2() = default;
    C2(const C &cIn) : c(cIn) {}
    ~C2();
    void operator()() const;
    void swap(C2 &o) { std::swap(c, o.c); }
};


template <typename T>
class Q {
    std::vector<T> queue;
public:
    void Add(std::vector<T> &items) {
        for (T & item : items) {
            queue.push_back(T());
            item.swap(queue.back());
        }
    }
    void Exec();
};

extern void foo(Q<C2> & q, std::vector<C2> &items);
void foo(Q<C2> & q, std::vector<C2> &items) { q.Add(items); q.Exec(); }
