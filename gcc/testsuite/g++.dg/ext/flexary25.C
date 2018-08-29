// PR c++/81922
// { dg-do compile }
// { dg-options "" }

struct S { const char *a; char b[]; };
struct T { int a; int b[]; };
#if __cplusplus >= 201103L
S c[] { "", "" };		// { dg-error "initialization of flexible array member in a nested context" "" { target c++11 } }
S d[] { "", { 0 } };		// { dg-error "initialization of flexible array member in a nested context" "" { target c++11 } }
T e[] { 1, { 2 }, 3, { 4 } };	// { dg-error "initialization of flexible array member in a nested context" "" { target c++11 } }
T f[] { 1, {}, 3, {} };
T g { 1, { 1, 2, 3 } };
S h { "abcd", "" };
#endif
S i[] = { "", "", "", "" };	// { dg-error "initialization of flexible array member in a nested context" }
S j[] = { "", { 1 }, "", { 2, 3 } };	// { dg-error "initialization of flexible array member in a nested context" }
T k[] = { 1, { 2 }, 3, { 4 } };	// { dg-error "initialization of flexible array member in a nested context" }
T l[] = { 1, {}, 3, {} };
T m = { 1, { 1, 2, 3 } };
S n = { "", "abcde" };
