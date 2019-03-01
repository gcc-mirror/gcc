// PR c++/89315
// { dg-do compile { target c++11 } }

#include <initializer_list>

struct bar {
    bar(std::initializer_list<int>, int = int());
};

struct i {
    const bar & invitees;
};

template <typename = void> struct n {
public:
    void m_fn1() { i{{}}; }
};

struct o : n<> {
    void p() { m_fn1(); }
};
