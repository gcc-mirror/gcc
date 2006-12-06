// Bug: 23225

void Dispatcher()
	 (__builtin_offsetof (ArgsType, largeMsgLen))
	/* { dg-error "function " "function" { target *-*-* } 4 } */
