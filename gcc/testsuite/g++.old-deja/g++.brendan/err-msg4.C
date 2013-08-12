// { dg-do assemble  }
// GROUPS passed error-messages
class X {
public:
    static int x;// { dg-message "" }  previous.*
    static int y;// { dg-message "" }  previous.*
};

unsigned X::x;// { dg-error "" }  conflict.*
unsigned X::y;// { dg-error "" }  conflict.*
