// { dg-do assemble  }
// GROUPS passed nested-classes
// This is the first line of file ||t5.C||.

// This code initializing an unnamed union inside a class appears to
// be legal C++ input and compiles fine with AT&T cfront 3.0.1, but
// gcc 2.4.5 complains about multiple initializations of the same
// member.

class Expr {
public:
    enum Type { undefined, slong, ulong, ldouble };
    Expr();
    Expr(Type type, const Expr* initializer);
private:
    Type type_;
    union {
	long slong_;
	unsigned long ulong_;
	long double ldouble_;
    };
};

// Construct an undefined expression.

Expr::Expr()
    :
    type_(undefined),
    slong_(-1)
{}
