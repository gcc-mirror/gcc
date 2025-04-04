// PR preprocessor/119391
// { dg-do preprocess }
// { dg-options "" }

#if (1 << 63) != -9223372036854775807 - 1	// { dg-warning "integer overflow in preprocessor expression" "" { target c++98_only } }
#warning "Unexpected value"
#endif
#if (3 << 62) != -4611686018427387904		// { dg-warning "integer overflow in preprocessor expression" "" { target c++98_only } }
#warning "Unexpected value"
#endif
#if 1 << 64					// { dg-warning "integer overflow in preprocessor expression" }
#endif
#if (3 << 63) != -9223372036854775807 - 1	// { dg-warning "integer overflow in preprocessor expression" "" { target c++17_down } }
#warning "Unexpected value"
#endif
