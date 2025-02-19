// PR c++/118306 - "Document" various behaviours wrt. defaulting types to int.
// { dg-do "compile" }
// { dg-additional-options "-fpermissive" }

// Members.
struct K {
  * mem1;	    // { dg-warning "forbids declaration" }
  * mem2;	    // { dg-warning "forbids declaration" }
  const * mem3;	    // { dg-warning "forbids declaration" }
  const ** mem4;    // { dg-warning "forbids declaration" }
  & mem5;	    // { dg-warning "forbids declaration" }
  volatile & mem6;  // { dg-warning "forbids declaration" }

  void foo (const& permissive_fine,		// { dg-warning "forbids declaration" }
	    volatile* permissive_fine_as_well); // { dg-warning "forbids declaration" }

  * bar () { return 0; }  // { dg-warning "forbids declaration" }
  const& baz ();	  // { dg-warning "forbids declaration" }

  void bazz () {
    try {}
    catch (const *i) {}	// { dg-warning "forbids" }
    catch (const &i) {}	// { dg-warning "forbids" }
  }
};

// Template parameters.
template<const *i, const &j>  // { dg-warning "forbids" }
void baz() {}

// Functions.
foo(int) { return 42; }		    // { dg-warning "forbids declaration" }
*bar(int) { return 0; }		    // { dg-warning "forbids declaration" }
**bazz(int) { return 0; }	    // { dg-warning "forbids declaration" }
*&bazzz(int) { return 0; }	    // { dg-warning "forbids declaration|bind non-const" }
const bazzzz (int) { return 0; }    // { dg-warning "forbids declaration" }
const* bazzzzz (int) { return 0; }  // { dg-warning "forbids declaration" }
