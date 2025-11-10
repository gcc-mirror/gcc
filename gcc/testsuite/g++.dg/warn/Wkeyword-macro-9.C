// C++26 P2843R3 - Preprocessing is never undefined
// [cpp.replace.general]/9
// { dg-do preprocess }
// { dg-options "-pedantic-errors" }

#undef for				// { dg-error "undefining keyword 'for'" "" { target c++26 } }
#define for for				// { dg-error "keyword 'for' defined as macro" "" { target c++26 } }
#undef for				// { dg-error "undefining keyword 'for'" "" { target c++26 } }
#define while do			// { dg-error "keyword 'while' defined as macro" "" { target c++26 } }
#define while do			// { dg-error "keyword 'while' defined as macro" "" { target c++26 } }
#define while for			// { dg-error "keyword 'while' defined as macro" "" { target c++26 } }
					// { dg-error "'while' redefined" "" { target *-*-* } .-1 }
#undef while				// { dg-error "undefining keyword 'while'" "" { target c++26 } }
#define while while			// { dg-error "keyword 'while' defined as macro" "" { target c++26 } }
#define private public			// { dg-error "keyword 'private' defined as macro" "" { target c++26 } }
#define inline				// { dg-error "keyword 'inline' defined as macro" "" { target c++26 } }
#undef inline				// { dg-error "undefining keyword 'inline'" "" { target c++26 } }
#define inline __inline__ __attribute__((__always_inline__))	// { dg-error "keyword 'inline' defined as macro" "" { target c++26 } }
#define likely(a) a
#undef likely
#define unlikely(a, b, c) a + b + c
#define unlikely(a, b, c) a + b + c
