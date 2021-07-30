// PR c++/101582
// { dg-do compile }
// { dg-options "" }

;
[[]] [[]] [[]];	// { dg-warning "attributes only available with" "" { target c++98_only } }
[[foobar]];	// { dg-warning "attribute ignored" }
// { dg-warning "attributes only available with" "" { target c++98_only } .-1 }

extern "C" ;
extern "C" [[]];	// { dg-warning "attributes only available with" "" { target c++98_only } }
extern "C" extern "C" ;
extern "C" extern "C" [[]][[]][[]];	// { dg-warning "attributes only available with" "" { target c++98_only } }
__extension__ ;
__extension__ [[]];			// { dg-warning "attributes only available with" "" { target c++98_only } }
__extension__ __extension__ ;
__extension__ __extension__ [[]][[]];	// { dg-warning "attributes only available with" "" { target c++98_only } }

namespace N {

;
[[]] [[]] [[]];	// { dg-warning "attributes only available with" "" { target c++98_only } }
[[foobar]];	// { dg-warning "attribute ignored" }
// { dg-warning "attributes only available with" "" { target c++98_only } .-1 }

extern "C" ;
extern "C" [[]];	// { dg-warning "attributes only available with" "" { target c++98_only } }
extern "C" extern "C" ;
extern "C" extern "C" [[]][[]][[]];	// { dg-warning "attributes only available with" "" { target c++98_only } }
__extension__ ;
__extension__ [[]];			// { dg-warning "attributes only available with" "" { target c++98_only } }
__extension__ __extension__ ;
__extension__ __extension__ [[]][[]];	// { dg-warning "attributes only available with" "" { target c++98_only } }

}
