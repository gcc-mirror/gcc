// Bug: 23225

void Dispatcher()
	 (__builtin_offsetof (ArgsType, largeMsgLen)) // { dg-error "initialize" }
// { dg-error "54:expected" "" { target *-*-* } .-1 }
