double foo [] =
{ &bar,		/* { dg-error "undeclared|is not constant|near init" } */
  (void *) 0 };	/* { dg-error "incompatible types|is not constant|near init" } */
double baz [] =
{ (void *) 0 };	/* { dg-error "incompatible types|is not constant|near init" } */
