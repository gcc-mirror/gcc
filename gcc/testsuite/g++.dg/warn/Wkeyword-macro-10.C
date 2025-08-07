// C++26 P2843R3 - Preprocessing is never undefined
// [cpp.replace.general]/9
// { dg-do preprocess }
// { dg-options "-Wkeyword-macro" }

#undef for				// { dg-warning "undefining keyword 'for'" }
#define for for				// { dg-warning "keyword 'for' defined as macro" }
#undef for				// { dg-warning "undefining keyword 'for'" }
#define while do			// { dg-warning "keyword 'while' defined as macro" }
#define while do			// { dg-warning "keyword 'while' defined as macro" }
#define while for			// { dg-warning "keyword 'while' defined as macro" }
					// { dg-warning "'while' redefined" "" { target *-*-* } .-1 }
#undef while				// { dg-warning "undefining keyword 'while'" }
#define while while			// { dg-warning "keyword 'while' defined as macro" }
#define private public			// { dg-warning "keyword 'private' defined as macro" }
#define inline				// { dg-warning "keyword 'inline' defined as macro" }
#undef inline				// { dg-warning "undefining keyword 'inline'" }
#define inline __inline__ __attribute__((__always_inline__))	// { dg-warning "keyword 'inline' defined as macro" }
#define likely(a) a
#undef likely				// { dg-warning "undefining keyword 'likely'" "" { target c++20 } }
#define unlikely(a, b, c) a + b + c
#define unlikely(a, b, c) a + b + c
#undef unlikely				// { dg-warning "undefining keyword 'unlikely'" "" { target c++20 } }
