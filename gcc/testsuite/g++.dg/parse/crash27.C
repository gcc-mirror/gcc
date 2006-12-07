// Bug: 23225

void Dispatcher()
	 (__builtin_offsetof (ArgsType, largeMsgLen))
	/* { dg-error "function " "function" { target *-*-* } 4 } */
	/* { dg-error "initializer" "expected 1" { target *-*-* } 4 } */
	/* { dg-error "expected ',' or" "expected 2" { target *-*-* } 4 } */
