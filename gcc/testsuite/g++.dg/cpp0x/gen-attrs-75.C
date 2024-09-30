// PR c++/101582
// { dg-do compile }
// { dg-options "-pedantic -Wno-extra-semi" }

;
[[]] [[]] [[]];	// { dg-warning "attributes only available with" "" { target c++98_only } }
[[foobar]];	// { dg-warning "attribute ignored" }
// { dg-warning "attributes only available with" "" { target c++98_only } .-1 }

extern "C" ;
extern "C" [[]];	// { dg-warning "attributes only available with" "" { target c++98_only } }
extern "C" extern "C" ;
extern "C" extern "C" [[]][[]][[]];	// { dg-warning "attributes only available with" "" { target c++98_only } }
__extension__ ;
__extension__ [[]];
__extension__ __extension__ ;
__extension__ __extension__ [[]][[]];

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
__extension__ [[]];
__extension__ __extension__ ;
__extension__ __extension__ [[]][[]];

}
