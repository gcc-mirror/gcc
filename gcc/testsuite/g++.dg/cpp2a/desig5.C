// { dg-do compile }
// { dg-options "" }

union u { int a; const char* b; };
u a = { 1 };
u b = a;
u c = 1;			// { dg-error "conversion from 'int' to non-scalar type 'u' requested" }
u d = { 0, "asdf" };		// { dg-error "too many initializers for" }
u e = { "asdf" };		// { dg-error "invalid conversion from 'const char\\*' to 'int'" }
u f = { .b = "asdf" };
u g = { .a = 1, .b = "asdf" };	// { dg-error "too many initializers for" }
