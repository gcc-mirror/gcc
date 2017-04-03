// PR c++/79782
// { dg-do compile { target c++11 } }
// { dg-options "-Wunused-but-set-parameter -Wunused-parameter" }

struct E { virtual E *foo () const = 0; };
struct F : virtual public E { };
struct G : public virtual F { G (int x) : F () { } };				// { dg-warning "unused parameter" }
struct H : virtual public E { H (int x, int y); };
struct I : public virtual H { I (int x, int y) : H (x, y) { } };		// { dg-bogus "set but not used" }
struct J : public virtual H { J (int x, int y) : H { x, y } { } };		// { dg-bogus "set but not used" }
struct K : public virtual H { K (int x, int y) : H (x * 0, y + 1) { } };	// { dg-bogus "set but not used" }
struct L : public virtual H { L (int x, int y) : H { x & 0, y | 1 } { } };	// { dg-bogus "set but not used" }
