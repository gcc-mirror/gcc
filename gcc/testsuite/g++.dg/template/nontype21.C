// PR c++/47705

template<char const * const x> class Something { };

extern char const xyz;

class SomethingElse:public Something<xyz> { }; // { dg-error "" }
