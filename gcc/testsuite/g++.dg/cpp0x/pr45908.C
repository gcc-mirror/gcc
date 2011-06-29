// PR c++/45908
// Testcase by Jonathan Wakely <redi@gcc.gnu.org>

// { dg-do compile }
// { dg-options "-std=c++0x" }

struct vector {
    struct iterator { };
    struct const_iterator { };
    iterator begin();
    const_iterator begin() const;
};

class block {
    vector v;
    auto end() const -> decltype(v.begin())
    { return v.begin(); }
};
