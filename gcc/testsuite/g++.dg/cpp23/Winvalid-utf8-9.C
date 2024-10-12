// P2295R6 - Support for UTF-8 as a portable source file encoding
// This test intentionally contains various byte sequences which are not valid UTF-8
// { dg-do preprocess }
// { dg-options "-finput-charset=UTF-8" }

#define I(x)
I(Â€ß¿à €íŸ¿î€€ğ€€ô¿¿)	// { dg-bogus "invalid UTF-8 character" }
                                // { dg-error "is not valid in an identifier" "" { target *-*-* } .-1 }
I(€)				// { dg-warning "invalid UTF-8 character '<80>'" "" { target c++23 } }
I(¿)				// { dg-warning "invalid UTF-8 character '<bf>'" "" { target c++23 } }
I(À)				// { dg-warning "invalid UTF-8 character '<c0>'" "" { target c++23 } }
I(Á)				// { dg-warning "invalid UTF-8 character '<c1>'" "" { target c++23 } }
I(õ)				// { dg-warning "invalid UTF-8 character '<f5>'" "" { target c++23 } }
I(ÿ)				// { dg-warning "invalid UTF-8 character '<ff>'" "" { target c++23 } }
I(Â)				// { dg-warning "invalid UTF-8 character '<c2>'" "" { target c++23 } }
I(à)				// { dg-warning "invalid UTF-8 character '<e0>'" "" { target c++23 } }
I(à€¿)				// { dg-warning "invalid UTF-8 character '<e0><80><bf>'" "" { target c++23 } }
I(àŸ€)				// { dg-warning "invalid UTF-8 character '<e0><9f><80>'" "" { target c++23 } }
I(à¿)				// { dg-warning "invalid UTF-8 character '<e0><bf>'" "" { target c++23 } }
I(ì€)				// { dg-warning "invalid UTF-8 character '<ec><80>'" "" { target c++23 } }
I(í €)				// { dg-warning "invalid UTF-8 character '<ed><a0><80>'" "" { target c++23 } }
I(ğ€€€)				// { dg-warning "invalid UTF-8 character '<f0><80><80><80>'" "" { target c++23 } }
I(ğ¿¿)				// { dg-warning "invalid UTF-8 character '<f0><8f><bf><bf>'" "" { target c++23 } }
I(ô€€)				// { dg-error "is not valid in an identifier" }
I(ı¿¿¿¿¿)			// { dg-error "is not valid in an identifier" }
