// PR c++/44859

struct Base2 { int m_foo; };
struct Derived2 : public Base2 {};

const Base2& f8() { return Derived2(); } // { dg-message "reference to temporary" }

struct foo { };
struct bar { foo base; };

const foo& f9() { return bar().base; } // { dg-message "reference to temporary" }
