// { dg-do assemble  }
// GROUPS passed error-messages
class X {
public:
    static int x;// { dg-error "" }  previous.*
    static int y;// { dg-error "" }  previous.*
};

unsigned X::x;// { dg-error "" }  conflict.*
unsigned X::y;// { dg-error "" }  conflict.*
