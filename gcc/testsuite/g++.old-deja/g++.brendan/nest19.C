// Build don't link: 
// GROUPS passed nested-classes
// This is the first line of file ||t3.C||.

// This code demonstrates a variation of the same problem with nested
// types.  In C++, nested typedefs are not supposed to be visible
// outside their class scopes but they apparently are in gcc 2.4.5.
// This code compiles fine in AT&T cfront 3.0.1, but gcc rejects it
// with the messages given below.

// If this class does not precede Expr, then the code will compile.

class Another {
public:
    typedef int Boolean;
    enum { FALSE, TRUE };
};

// If Expr does not define typedef int Boolean, then the code will
// compile.

class Expr {
public:
    typedef int Boolean;
    enum { FALSE, TRUE };
    void foo();
    void call_something_with(Boolean);
};

// t3.C: In method `void  Expr::foo ()':
//   t3.C:36: uninitialized const `Boolean'
//   t3.C:36: parse error before `='
//   t3.C:37: `argument' undeclared (first use this function)
//   t3.C:37: (Each undeclared identifier is reported only once
//   t3.C:37: for each function it appears in.)

void Expr::foo() {
    const Boolean argument = TRUE;
    call_something_with(argument);
}
