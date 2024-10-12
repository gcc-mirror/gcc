// P2295R6 - Support for UTF-8 as a portable source file encoding
// This test intentionally contains various byte sequences which are not valid UTF-8
// { dg-do preprocess }
// { dg-options "-finput-charset=UTF-8 -Winvalid-utf8" }

#define I(x)
I(Â€ß¿à €íŸ¿î€€ğ€€ô¿¿)	// { dg-bogus "invalid UTF-8 character" }
                                // { dg-error "is not valid in an identifier" "" { target c++ } .-1 }
I(€)				// { dg-warning "invalid UTF-8 character '<80>'" }
I(¿)				// { dg-warning "invalid UTF-8 character '<bf>'" }
I(À)				// { dg-warning "invalid UTF-8 character '<c0>'" }
I(Á)				// { dg-warning "invalid UTF-8 character '<c1>'" }
I(õ)				// { dg-warning "invalid UTF-8 character '<f5>'" }
I(ÿ)				// { dg-warning "invalid UTF-8 character '<ff>'" }
I(Â)				// { dg-warning "invalid UTF-8 character '<c2>'" }
I(à)				// { dg-warning "invalid UTF-8 character '<e0>'" }
I(à€¿)				// { dg-warning "invalid UTF-8 character '<e0><80><bf>'" }
I(àŸ€)				// { dg-warning "invalid UTF-8 character '<e0><9f><80>'" }
I(à¿)				// { dg-warning "invalid UTF-8 character '<e0><bf>'" }
I(ì€)				// { dg-warning "invalid UTF-8 character '<ec><80>'" }
I(í €)				// { dg-warning "invalid UTF-8 character '<ed><a0><80>'" }
I(ğ€€€)				// { dg-warning "invalid UTF-8 character '<f0><80><80><80>'" }
I(ğ¿¿)				// { dg-warning "invalid UTF-8 character '<f0><8f><bf><bf>'" }
I(ô€€)				// { dg-warning "invalid UTF-8 character '<f4><90><80><80>'" "" { target c } }
                                // { dg-error "is not valid in an identifier" "" { target c++ } .-1 }
I(ı¿¿¿¿¿)			// { dg-warning "invalid UTF-8 character '<fd><bf><bf><bf>'" "" { target c } }
                                // { dg-error "is not valid in an identifier" "" { target c++ } .-1 }
