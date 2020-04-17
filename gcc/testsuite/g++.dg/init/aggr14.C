// PR c++/93790 - wrong paren-init of aggregates interference.
// { dg-do compile }

struct S {};
class S_refwrap {
    S& Sref_;
public:
    S_refwrap(S& Sref) : Sref_(Sref) {}
    operator S&() { return Sref_; }
};

S s;
S_refwrap r(s);
S& s2(r);
